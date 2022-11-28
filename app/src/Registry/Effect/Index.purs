module Registry.Effect.Index where

import Prelude
import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Foreign.FastGlob as FastGlob
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Json as Json
import Registry.Constants as Constants
import Registry.Effect.Cache (CACHE)
import Registry.Effect.Cache as Cache
import Registry.Effect.GitHub (REGISTRY_REPO)
import Registry.Effect.GitHub as GitHub
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))

data Index a
  = ReadMetadataIndex (Map PackageName Metadata -> a)
  | ReadMetadata PackageName (Maybe Metadata -> a)
  | WriteMetadata PackageName Metadata a
  | ReadManifestIndex (ManifestIndex -> a)
  | ReadManifest PackageName Version (Maybe Manifest -> a)
  | WriteManifest Manifest a
  | DeleteManifest PackageName Version a

derive instance Functor Index

-- | An effect for interacting with Registry indices of package manifests and
-- | package metadata.
type INDEX r = (index :: Index | r)

_index :: Proxy "index"
_index = Proxy

readMetadataIndex :: forall r. Run (INDEX + r) (Map PackageName Metadata)
readMetadataIndex = Run.lift _index (ReadMetadataIndex identity)

readMetadata :: forall r. PackageName -> Run (INDEX + r) (Maybe Metadata)
readMetadata name = Run.lift _index (ReadMetadata name identity)

writeMetadata :: forall r. PackageName -> Metadata -> Run (INDEX + r) Unit
writeMetadata name metadata = Run.lift _index (WriteMetadata name metadata unit)

readManifestIndex :: forall r. Run (INDEX + r) ManifestIndex
readManifestIndex = Run.lift _index (ReadManifestIndex identity)

readManifest :: forall r. PackageName -> Version -> Run (INDEX + r) (Maybe Manifest)
readManifest name version = Run.lift _index (ReadManifest name version identity)

writeManifest :: forall r. Manifest -> Run (INDEX + r) Unit
writeManifest manifest = Run.lift _index (WriteManifest manifest unit)

deleteManifest :: forall r. PackageName -> Version -> Run (INDEX + r) Unit
deleteManifest name version = Run.lift _index (DeleteManifest name version unit)

type IndexEnv =
  { registryPath :: FilePath
  , registryIndexPath :: FilePath
  }

handleIndex :: forall r a. IndexEnv -> Index a -> Run (REGISTRY_REPO + CACHE + LOG + AFF + r) a
handleIndex { registryPath, registryIndexPath } = case _ of
  ReadMetadataIndex reply -> do
    let metadataDir = Path.concat [ registryPath, Constants.packageMetadataDirectory ]

    files <- Run.liftAff $ FS.Aff.readdir metadataDir
    let stripSuffix = note "No .json suffix" <<< String.stripSuffix (String.Pattern ".json")
    let packages = partitionEithers $ map (PackageName.parse <=< stripSuffix) files
    unless (Array.null packages.fail) do
      Log.die $ "Some entries in the metadata directory are not valid package names:" <> Array.foldMap (append "\n  - ") packages.fail

    entries <- Run.liftAff $ map partitionEithers $ for packages.success \name -> do
      result <- Json.readJsonFile Metadata.codec (Path.concat [ registryPath, Metadata.packageMetadataPath name ])
      pure $ map (Tuple name) result
    unless (Array.null entries.fail) do
      Log.die $ append "Invalid metadata directory (some package metadata cannot be decoded):" $ Array.foldMap (append "\n  - ") entries.fail

    Log.debug "Successfully read metadata entries."
    pure $ reply $ Map.fromFoldable entries.success

  ReadMetadata name reply -> do
    Log.debug $ "Reading metadata for " <> PackageName.print name
    let path = Path.concat [ registryPath, Metadata.packageMetadataPath name ]
    Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
      Left fsError -> do
        Log.debug $ "Could not find metadata file: " <> Aff.message fsError
        pure $ reply Nothing
      Right contents -> case Json.jsonParser contents of
        Left jsonError -> Log.die $ "Metadata file is not valid JSON: " <> jsonError
        Right parsed -> case Json.decode Metadata.codec parsed of
          Left decodeError -> Log.die $ "Metadata JSON could not be parsed: " <> Json.printJsonDecodeError decodeError
          Right metadata -> do
            Log.debug $ "Successfully read metadata for " <> PackageName.print name
            pure $ reply $ Just metadata

  WriteMetadata name metadata next -> do
    Log.debug $ "Writing metadata for " <> PackageName.print name
    let path = Path.concat [ registryPath, Metadata.packageMetadataPath name ]
    Run.liftAff (Aff.attempt (Json.writeJsonFile Metadata.codec path metadata)) >>= case _ of
      Left fsError -> Log.die $ "Could not write metadata file to " <> path <> ": " <> Aff.message fsError
      Right _ -> GitHub.commitMetadata name
    pure next

  ReadManifestIndex reply -> do
    Log.debug $ "Reading manifest index from " <> registryIndexPath
    paths <- Run.liftAff $ FastGlob.match' registryIndexPath [ "**/*" ] { include: FastGlob.FilesOnly, ignore: [ "config.json" ] }
    let packages = partitionEithers $ map (PackageName.parse <<< Path.basename) paths.succeeded
    Log.warn $ "Some entries in the manifest index are not valid package names:" <> Array.foldMap (append "\n  - ") packages.fail
    entries <- Run.liftAff $ map partitionEithers $ for packages.success (ManifestIndex.readEntryFile registryIndexPath)
    case entries.fail of
      [] -> case ManifestIndex.fromSet $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray entries.success of
        Left errors -> Log.die $ append "Invalid ManifestIndex (some packages are not satisfiable):" $ Array.foldMap (append "\n  - ") do
          Tuple name versions <- Map.toUnfoldable errors
          Tuple version dependency <- Map.toUnfoldable versions
          let
            dependencies = do
              Tuple depName depRange <- Map.toUnfoldable dependency
              [ PackageName.print depName <> "(" <> Range.print depRange <> ")" ]
          pure $ Array.fold [ formatPackageVersion name version, " cannot satisfy: ", String.joinWith ", " dependencies ]

        Right index ->
          pure $ reply index

      failed ->
        Log.die $ append "Invalid ManifestIndex (some package entries cannot be decoded):" $ Array.foldMap (append "\n  - ") failed

  ReadManifest name version reply -> do
    Log.debug $ "Reading manifest for " <> formatPackageVersion name version <> "..."
    Cache.get (Cache.ManifestFile name version) >>= case _ of
      Nothing -> do
        Run.liftAff (ManifestIndex.readEntryFile registryIndexPath name) >>= case _ of
          Left error -> do
            Log.debug $ "Could not find any entries for package " <> PackageName.print name <> ": " <> error
            pure $ reply Nothing
          Right entries ->
            case NonEmptyArray.find (\(Manifest m) -> m.name == name && m.version == version) entries of
              Nothing -> do
                Log.debug $ "Found entries for package " <> PackageName.print name <> " but none for version " <> Version.print version
                pure $ reply Nothing
              Just entry -> do
                Cache.put (Cache.ManifestFile name version) entry
                pure $ reply $ Just entry
      Just cached ->
        pure $ reply $ Just cached.value

  WriteManifest manifest@(Manifest { name, version }) next -> do
    Log.debug $ "Writing manifest for " <> formatPackageVersion name version
    Run.liftAff (ManifestIndex.insertIntoEntryFile registryIndexPath manifest) >>= case _ of
      Left error -> Log.die $ "Could not write manifest file: " <> error
      Right _ -> do
        Cache.put (Cache.ManifestFile name version) manifest
        GitHub.commitManifest name version
        pure next

  DeleteManifest name version next -> do
    Log.debug $ "Deleting manifest for " <> formatPackageVersion name version
    Run.liftAff (ManifestIndex.removeFromEntryFile registryIndexPath name version) >>= case _ of
      Left error -> Log.die $ "Could not remove manifest file: " <> error
      Right _ -> do
        Cache.delete (Cache.ManifestFile name version)
        GitHub.commitManifest name version
        pure next
