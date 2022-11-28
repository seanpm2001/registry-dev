module Registry.Legacy.Manifest where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.Either as Either
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.These (These(..))
import Data.These as These
import Data.Variant as Variant
import Foreign.GitHub as GitHub
import Foreign.Licensee as Licensee
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.App.Json as Json
import Registry.App.Types (LegacyPackageSet(..), LegacyPackageSetEntry, LegacyPackageSetUnion)
import Registry.App.Types as Types
import Registry.Effect.Cache (CACHE)
import Registry.Effect.Cache as Cache
import Registry.Effect.GitHub (GITHUB)
import Registry.Effect.GitHub as Effect.GitHub
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.Internal.Codec as Internal.Codec
import Registry.Legacy.LenientRange as LenientRange
import Registry.Legacy.LenientVersion as LenientVersion
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.License as License
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except as Run.Except
import Sunde as Process
import Type.Proxy (Proxy(..))

type LegacyManifest =
  { license :: License
  , description :: Maybe String
  , dependencies :: Map PackageName Range
  }

toManifest :: PackageName -> Version -> Location -> LegacyManifest -> Manifest
toManifest name version location { license, description, dependencies } = do
  let files = Nothing
  let owners = Nothing
  Manifest { name, version, location, license, description, dependencies, files, owners }

fetchLegacyManifest
  :: forall r
   . Maybe (Map PackageName RawVersion)
  -> GitHub.Address
  -> RawVersion
  -> Run (GITHUB + LOG + AFF + r) (Either LegacyManifestValidationError LegacyManifest)
fetchLegacyManifest packageSetsDeps address ref = Run.Except.runExcept do
  manifests <- fetchLegacyManifestFiles address ref >>= Run.Except.rethrow

  dependencies <- do
    let
      -- Package sets will produce exact versions for dependencies, ie. a
      -- dependency on "strings: 4.1.2" will produce the version range
      -- "strings: >=4.1.2 <4.1.3", which while technically correct is almost
      -- certainly not what the user wants. We instead treat these ranges as
      -- caret ranges, so "strings: 4.1.2" becomes "strings: >=4.1.2 <5.0.0".
      fixedToRange (RawVersion fixed) = do
        let parsedVersion = LenientVersion.parse fixed
        let bump version = Version.print (Version.bumpHighest version)
        let printRange version = Array.fold [ ">=", fixed, " <", bump version ]
        RawVersionRange $ Either.either (const fixed) (printRange <<< LenientVersion.version) parsedVersion

      validatePackageSetDeps = case packageSetsDeps of
        Nothing ->
          Left { error: InvalidDependencies [], reason: "No package sets dependencies." }
        Just deps -> do
          let converted = mapKeys (RawPackageName <<< PackageName.print) $ map fixedToRange deps
          validateDependencies converted

      convertSpagoDeps packages = Array.foldl foldFn Map.empty
        where
        foldFn deps name = maybe deps (\{ version } -> Map.insert name (fixedToRange version) deps) (findPackage name)
        findPackage name = Map.lookup name packages

      validateSpagoDeps (SpagoDhallJson { dependencies, packages }) = do
        validateDependencies (convertSpagoDeps packages dependencies)

      -- We remove dependencies that don't begin with 'purescript-', as these
      -- indicate JavaScript dependencies.
      convertBowerDeps = Map.mapMaybeWithKey \(RawPackageName name) range -> do
        _ <- String.stripPrefix (String.Pattern "purescript-") name
        pure range

      validateBowerDeps (Bowerfile { dependencies }) =
        validateDependencies (convertBowerDeps dependencies)

      unionManifests = do
        case manifests of
          This bower -> validateBowerDeps bower
          That spago -> validateSpagoDeps spago
          Both bower spago ->
            case validateBowerDeps bower, validateSpagoDeps spago of
              Left bowerError, Left _ -> Left bowerError
              Right bowerDeps, Left _ -> Right bowerDeps
              Left _, Right spagoDeps -> Right spagoDeps
              Right bowerDeps, Right spagoDeps -> Right do
                bowerDeps # mapWithIndex \package range ->
                  case Map.lookup package spagoDeps of
                    Nothing -> range
                    Just spagoRange -> Range.union range spagoRange

      unionPackageSets = case validatePackageSetDeps, unionManifests of
        Left _, Left manifestError -> Left manifestError
        Left _, Right manifestDeps -> Right manifestDeps
        Right packageSetDeps, Left _ -> Right packageSetDeps
        Right packageSetDeps, Right manifestDeps -> Right do
          packageSetDeps # mapWithIndex \package range ->
            case Map.lookup package manifestDeps of
              Nothing -> range
              Just manifestRange -> Range.union range manifestRange

    Run.Except.rethrow unionPackageSets

  license <- do
    let unBower (Bowerfile { license }) = Array.mapMaybe NonEmptyString.fromString license
    let unSpago (SpagoDhallJson { license }) = Array.catMaybes [ license ]
    let manifestLicenses = These.these unBower unSpago (\bower spago -> unBower bower <> unSpago spago) manifests
    detectedLicenses <- detectLicenses address ref
    let licenses = Array.nub $ Array.concat [ detectedLicenses, manifestLicenses ]
    Run.Except.rethrow $ validateLicense licenses

  let
    description = do
      Bowerfile fields <- These.theseLeft manifests
      fields.description

  pure { license, dependencies, description }

type LegacyManifestValidationError = { error :: LegacyManifestError, reason :: String }

data LegacyManifestError
  = NoManifests
  | MissingLicense
  | InvalidLicense (Array String)
  | InvalidDependencies (Array { name :: String, range :: String, error :: String })

legacyManifestErrorCodec :: JsonCodec LegacyManifestError
legacyManifestErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { noManifests: Left unit
  , missingLicense: Left unit
  , invalidLicense: Right (CA.array CA.string)
  , invalidDependencies: Right (CA.array dependencyCodec)
  }
  where
  dependencyCodec = Json.object "Dependency"
    { name: CA.string
    , range: CA.string
    , error: CA.string
    }

  toVariant = case _ of
    NoManifests -> Variant.inj (Proxy :: _ "noManifests") unit
    MissingLicense -> Variant.inj (Proxy :: _ "missingLicense") unit
    InvalidLicense inner -> Variant.inj (Proxy :: _ "invalidLicense") inner
    InvalidDependencies inner -> Variant.inj (Proxy :: _ "invalidDependencies") inner

  fromVariant = Variant.match
    { noManifests: \_ -> NoManifests
    , missingLicense: \_ -> MissingLicense
    , invalidLicense: InvalidLicense
    , invalidDependencies: InvalidDependencies
    }

printLegacyManifestError :: LegacyManifestError -> String
printLegacyManifestError = case _ of
  NoManifests -> "NoManifests"
  MissingLicense -> "MissingLicense"
  InvalidLicense licenses -> "InvalidLicense (" <> String.joinWith ", " licenses <> ")"
  InvalidDependencies errors -> "InvalidDependencies (" <> String.joinWith ", " (map printDepError errors) <> ")"
  where
  printDepError { name, range, error } = "[{ " <> name <> ": " <> range <> "}, " <> error <> "]"

fetchLegacyManifestFiles
  :: forall r
   . GitHub.Address
  -> RawVersion
  -> Run (GITHUB + LOG + AFF + r) (Either LegacyManifestValidationError (These Bowerfile SpagoDhallJson))
fetchLegacyManifestFiles address ref = do
  Log.debug $ "Fetching legacy manifest files for " <> show address <> " at ref " <> un RawVersion ref
  eitherBower <- fetchBowerfile address ref
  eitherSpago <- fetchSpagoDhallJson address ref
  pure $ case eitherBower, eitherSpago of
    Left _, Left _ -> Left { error: NoManifests, reason: "No bower.json or spago.dhall files available." }
    Right bower, Left _ -> Right $ This bower
    Left _, Right spago -> Right $ That spago
    Right bower, Right spago -> Right $ Both bower spago

detectLicenses :: forall r. GitHub.Address -> RawVersion -> Run (GITHUB + LOG + AFF + r) (Array NonEmptyString)
detectLicenses address (RawVersion ref) = do
  Log.debug $ "Detecting licenses for " <> show address <> " at ref " <> ref
  packageJsonFile <- Effect.GitHub.getContent address ref "package.json"
  licenseFile <- Effect.GitHub.getContent address ref "LICENSE"
  let packageJsonInput = { name: "package.json", contents: _ } <$> hush packageJsonFile
  let licenseInput = { name: "LICENSE", contents: _ } <$> hush licenseFile
  Run.liftAff (Licensee.detectFiles (Array.catMaybes [ packageJsonInput, licenseInput ])) >>= case _ of
    Left err -> Log.debug ("Licensee decoding error, ignoring: " <> err) $> []
    Right licenses -> pure $ Array.mapMaybe NonEmptyString.fromString licenses

validateLicense :: Array NonEmptyString -> Either LegacyManifestValidationError License
validateLicense licenses = do
  let
    rewrite = case _ of
      "Apache 2" -> "Apache-2.0"
      "Apache-2" -> "Apache-2.0"
      "Apache 2.0" -> "Apache-2.0"
      "BSD" -> "BSD-3-Clause"
      "BSD3" -> "BSD-3-Clause"
      "BSD-3" -> "BSD-3-Clause"
      "3-Clause BSD" -> "BSD-3-Clause"
      other -> other

    parsedLicenses =
      map (License.parse <<< rewrite)
        $ Array.filter (_ /= "LICENSE")
        $ map NonEmptyString.toString licenses

  case partitionEithers parsedLicenses of
    { fail: [], success: [] } -> Left { error: MissingLicense, reason: "No licenses found." }
    { fail: [], success } -> Right $ License.joinWith License.And success
    { fail } -> Left { error: InvalidLicense fail, reason: "Licenses found, but not valid SPDX identifiers." }

validateDependencies :: Map RawPackageName RawVersionRange -> Either LegacyManifestValidationError (Map PackageName Range)
validateDependencies dependencies = do
  let
    foldFn (RawPackageName name) acc (RawVersionRange range) = do
      let failWith = { name, range, error: _ }
      case PackageName.parse (stripPureScriptPrefix name), LenientRange.parse range of
        Left nameErr, Left rangeErr ->
          acc { no = Array.cons (failWith (nameErr <> rangeErr)) acc.no }
        Left nameErr, Right _ ->
          acc { no = Array.cons (failWith nameErr) acc.no }
        Right _, Left rangeErr ->
          acc { no = Array.cons (failWith rangeErr) acc.no }
        Right parsedName, Right parsedRange ->
          acc { yes = Array.cons (Tuple parsedName (LenientRange.range parsedRange)) acc.yes }

    parsedDependencies =
      foldlWithIndex foldFn { no: [], yes: [] } dependencies

  case parsedDependencies of
    { no: [], yes } -> pure $ Map.fromFoldable yes
    { no } -> Left { error: InvalidDependencies no, reason: "Version specifies invalid dependencies." }

-- | The result of calling 'dhall-to-json' on a 'spago.dhall' file and
-- | accompanying 'packages.dhall' file.
newtype SpagoDhallJson = SpagoDhallJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName { version :: RawVersion }
  }

derive instance Newtype SpagoDhallJson _

spagoDhallJsonCodec :: JsonCodec SpagoDhallJson
spagoDhallJsonCodec = Profunctor.dimap toRep fromRep $ Json.object "SpagoDhallJson"
  { license: CA.Record.optional CA.Common.nonEmptyString
  , dependencies: CA.Record.optional (CA.array (Profunctor.wrapIso RawPackageName CA.string))
  , packages: CA.Record.optional packageVersionMapCodec
  }
  where
  packageVersionMapCodec :: JsonCodec (Map RawPackageName { version :: RawVersion })
  packageVersionMapCodec = rawPackageNameMapCodec $ Json.object "VersionObject" { version: rawVersionCodec }

  toRep (SpagoDhallJson fields) = fields
    { dependencies = if Array.null fields.dependencies then Nothing else Just fields.dependencies
    , packages = if Map.isEmpty fields.packages then Nothing else Just fields.packages
    }

  fromRep fields = SpagoDhallJson $ fields
    { dependencies = fromMaybe [] fields.dependencies
    , packages = fromMaybe Map.empty fields.packages
    }

-- | Attempt to construct a SpagoDhallJson file from a spago.dhall and
-- | packages.dhall file located in a remote repository at the given ref.
fetchSpagoDhallJson :: forall r. GitHub.Address -> RawVersion -> Run (GITHUB + LOG + AFF + r) (Either GitHub.GitHubError SpagoDhallJson)
fetchSpagoDhallJson address (RawVersion ref) = Run.Except.runExcept do
  Log.debug $ "Fetching spago.dhall and packages.dhall for " <> show address <> " at ref " <> ref
  tmp <- Run.liftAff $ liftEffect Tmp.mkTmpDir
  spagoDhall <- Effect.GitHub.getContent address ref "spago.dhall" >>= Run.Except.rethrow
  packagesDhall <- Effect.GitHub.getContent address ref "packages.dhall" >>= Run.Except.rethrow
  Run.liftAff $ FSA.writeTextFile UTF8 (Path.concat [ tmp, "packages.dhall" ]) packagesDhall
  dhallJson <- Run.liftAff $ dhallToJson { dhall: spagoDhall, cwd: Just tmp }
  Run.Except.rethrow case dhallJson of
    Left err -> Left $ GitHub.DecodeError err
    Right json -> case Json.decodeJson spagoDhallJsonCodec json of
      Left err -> Left $ GitHub.DecodeError err
      Right value -> pure value
  where
  -- | Convert a string representing a Dhall expression into JSON using the
  -- | `dhall-to-json` CLI.
  dhallToJson :: { dhall :: String, cwd :: Maybe FilePath } -> Aff (Either String Json.Json)
  dhallToJson { dhall, cwd } = do
    let cmd = "dhall-to-json"
    let stdin = Just dhall
    let args = []
    result <- Process.spawn { cmd, stdin, args } (NodeProcess.defaultSpawnOptions { cwd = cwd })
    pure $ case result.exit of
      NodeProcess.Normally 0 -> Json.parseJson CA.json result.stdout
      _ -> Left result.stderr

newtype Bowerfile = Bowerfile
  { description :: Maybe String
  , dependencies :: Map RawPackageName RawVersionRange
  , license :: Array String
  }

derive instance Newtype Bowerfile _
derive newtype instance Eq Bowerfile

bowerfileCodec :: JsonCodec Bowerfile
bowerfileCodec = Profunctor.dimap toRep fromRep $ Json.object "Bowerfile"
  { description: CA.Record.optional CA.string
  , dependencies: CA.Record.optional dependenciesCodec
  , license: licenseCodec
  }
  where
  toRep (Bowerfile fields) = fields { dependencies = Just fields.dependencies }
  fromRep fields = Bowerfile $ fields { dependencies = fromMaybe Map.empty fields.dependencies }

  dependenciesCodec :: JsonCodec (Map RawPackageName RawVersionRange)
  dependenciesCodec = rawPackageNameMapCodec rawVersionRangeCodec

  licenseCodec :: JsonCodec (Array String)
  licenseCodec = CA.codec' decode encode
    where
    decode json = CA.decode (CA.array CA.string) json <|> map Array.singleton (CA.decode CA.string json)
    encode = CA.encode (CA.array CA.string)

-- | Attempt to construct a Bowerfile from a bower.json file located in a
-- | remote repository at the given ref.
fetchBowerfile :: forall r. GitHub.Address -> RawVersion -> Run (GITHUB + LOG + r) (Either GitHub.GitHubError Bowerfile)
fetchBowerfile address (RawVersion ref) = do
  Log.debug $ "Fetching bowerfile for " <> show address <> " at ref " <> ref
  Effect.GitHub.getJsonFile address ref bowerfileCodec "bower.json"

type LegacyPackageSetEntries = Map PackageName (Map RawVersion (Map PackageName RawVersion))

legacyPackageSetEntriesCodec :: JsonCodec LegacyPackageSetEntries
legacyPackageSetEntriesCodec = Internal.Codec.packageMap $ rawVersionMapCodec $ Internal.Codec.packageMap rawVersionCodec

fetchLegacyPackageSets :: forall r. Run (CACHE + GITHUB + LOG + AFF + r) LegacyPackageSetEntries
fetchLegacyPackageSets = do
  result <- Effect.GitHub.listTags Legacy.PackageSet.legacyPackageSetsRepo
  tags <- case result of
    Left err -> Log.die (GitHub.printGitHubError err)
    Right tags -> pure $ Legacy.PackageSet.filterLegacyPackageSets tags

  let
    convertEntry :: Map PackageName LegacyPackageSetEntry -> LegacyPackageSetEntry -> Map RawVersion (Map PackageName RawVersion)
    convertEntry entries { dependencies, version } = do
      let
        -- Package sets are only valid if all packages in the set have dependencies that are also in
        -- the set, so this (should) be safe.
        resolveDependencyVersion dep =
          unsafeFromJust (_.version <$> Map.lookup dep entries)
        resolveDependencyVersions =
          Array.foldl (\m name -> Map.insert name (resolveDependencyVersion name) m) Map.empty
      Map.singleton version (resolveDependencyVersions dependencies)

  tagsSha <- Run.liftAff $ liftEffect $ Sha256.hashString (String.joinWith " " tags)

  -- It's important that we cache the end result of unioning all package sets
  -- because the package sets are quite large and it's expensive to read them
  -- all into memory and fold over them.
  Cache.get (Cache.LegacyPackageSetUnion tagsSha) >>= case _ of
    Nothing -> do
      Log.debug $ "Building legacy package set union..."

      entries :: Array LegacyPackageSetUnion <- for tags \ref -> do
        legacySet <- Cache.get (Cache.LegacyPackageSet ref) >>= case _ of
          Nothing -> do
            Log.debug $ "Building legacy package set for " <> ref
            legacySet <- Effect.GitHub.getJsonFile Legacy.PackageSet.legacyPackageSetsRepo ref Types.legacyPackageSetCodec "packages.json"
            Cache.put (Cache.LegacyPackageSet ref) legacySet
            pure legacySet

          Just stored ->
            pure stored.value

        case legacySet of
          Left error -> do
            Log.warn $ "Skipping " <> ref <> " legacy package set because of an error " <> GitHub.printGitHubError error
            pure Map.empty
          Right (LegacyPackageSet set) ->
            pure $ map (convertEntry set) set

      let merged = Array.foldl (\m set -> Map.unionWith Map.union set m) Map.empty entries
      Log.debug $ "Successfully produced new union of legacy package sets."
      Cache.put (Cache.LegacyPackageSetUnion tagsSha) merged
      pure merged

    Just stored ->
      pure stored.value
