module Registry.Scripts.PackageSetUpdater where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Map as Map
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Registry.App.API as API
import Registry.App.Json as Json
import Registry.App.PackageIndex as PackageIndex
import Registry.App.PackageSets as App.PackageSets
import Registry.App.RegistryM (Env, RegistryEffects, RegistryM)
import Registry.App.RegistryM as RegistryM
import Registry.Effect.Log as Log
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Run (Run)
import Run as Run

data PublishMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PublishMode

main :: Effect Unit
main = Aff.launchAff_ do
  Console.log "Loading env..."
  _ <- API.loadEnv

  FS.Extra.ensureDirectory API.scratchDir

  Console.log "Parsing CLI args..."
  mode <- liftEffect do
    args <- Array.drop 2 <$> Node.Process.argv
    case Array.uncons args of
      Nothing -> Exception.throw "Expected 'generate' or 'commit', but received no arguments."
      Just { head, tail: [] } -> case head of
        "generate" -> pure GeneratePackageSet
        "commit" -> pure CommitPackageSet
        other -> Exception.throw $ "Expected 'generate' or 'commit' but received: " <> other
      Just _ -> Exception.throw $ String.joinWith "\n"
        [ "Expected 'generate' or 'commit', but received multiple arguments:"
        , String.joinWith " " args
        ]

  Console.log "Starting package set publishing..."

  githubToken <- liftEffect do
    Node.Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (Exception.throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      { closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , commitPackageSetFile: API.pacchettiBottiPushToRegistryPackageSets
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache: { write: mempty, read: \_ -> pure (Left mempty), remove: mempty }
      , username: mempty
      , packagesMetadata: metadataRef
      , registry: Path.concat [ API.scratchDir, "registry" ]
      , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
      }

    effects :: Run RegistryEffects ~> Aff
    effects =
      Log.runLogExcept
        >>> Run.interpret (Run.on Log._log (Log.handleLogFile (Path.concat [ API.scratchDir, "package-set-updater-logfile.txt" ])) Run.send)
        >>> Run.runBaseAff'

  RegistryM.runRegistryM env effects do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    registryIndex <- PackageIndex.readManifestIndexFromDisk
    prevPackageSet <- App.PackageSets.readLatestPackageSet
    App.PackageSets.validatePackageSet registryIndex prevPackageSet

    metadata <- RegistryM.readPackagesMetadata
    recentUploads <- findRecentUploads metadata (Hours 24.0)

    let candidates = App.PackageSets.validatePackageSetCandidates registryIndex prevPackageSet (map Just recentUploads.accepted)
    RegistryM.debug $ App.PackageSets.printRejections candidates.rejected

    if Map.isEmpty candidates.accepted then do
      RegistryM.info "No eligible additions, updates, or removals to produce a new package set."
    else do
      let
        logPackage name maybeVersion = case maybeVersion of
          -- There are no removals in the automated package sets. This should be
          -- an unreachable case.
          Nothing -> RegistryM.die "Package removals are not accepted in automatic package sets."
          Just version -> RegistryM.debug (PackageName.print name <> "@" <> Version.print version)

      RegistryM.info "Found the following package versions eligible for inclusion in package set:"
      forWithIndex_ candidates.accepted logPackage
      let workDir = Path.concat [ API.scratchDir, "package-set-build" ]
      App.PackageSets.processBatchSequential workDir registryIndex prevPackageSet Nothing candidates.accepted >>= case _ of
        Nothing -> do
          RegistryM.warn "\n----------\nNo packages could be added to the set. All packages failed:"
          forWithIndex_ candidates.accepted logPackage
        Just { success, fail, packageSet } -> do
          unless (Map.isEmpty fail) do
            RegistryM.warn "\n----------\nSome packages could not be added to the set:"
            forWithIndex_ fail logPackage
          RegistryM.info "\n----------\nNew packages were added to the set!"
          forWithIndex_ success logPackage
          newPath <- App.PackageSets.getPackageSetPath (un PackageSet packageSet).version
          liftAff $ Json.writeJsonFile PackageSet.codec newPath packageSet
          case mode of
            GeneratePackageSet -> pure unit
            CommitPackageSet -> do
              let commitMessage = App.PackageSets.commitMessage prevPackageSet success (un PackageSet packageSet).version
              RegistryM.commitPackageSetFile (un PackageSet packageSet).version commitMessage >>= case _ of
                Left err -> RegistryM.die $ "Failed to commit package set file: " <> err
                Right _ -> do
                  case Legacy.PackageSet.fromPackageSet registryIndex metadata packageSet of
                    Left err -> RegistryM.die err
                    Right converted -> Legacy.PackageSet.mirrorLegacySet converted

findRecentUploads :: Map PackageName Metadata -> Hours -> RegistryM { accepted :: Map PackageName Version, rejected :: Map PackageName (NonEmptyArray Version) }
findRecentUploads metadata limit = do
  now <- liftEffect nowUTC

  let
    packageUploads = Map.fromFoldable do
      Tuple packageName (Metadata packageMetadata) <- Map.toUnfoldable metadata
      versions <- Array.fromFoldable $ NonEmptyArray.fromArray do
        Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
        let diff = DateTime.diff now publishedTime
        guardA (diff <= limit)
        pure version
      pure (Tuple packageName versions)

    deduplicated = packageUploads # flip foldlWithIndex { rejected: Map.empty, accepted: Map.empty } \name acc versions -> do
      let { init, last } = NonEmptyArray.unsnoc versions
      case NonEmptyArray.fromArray init of
        Nothing -> acc { accepted = Map.insert name last acc.accepted }
        Just entries -> acc { accepted = Map.insert name last acc.accepted, rejected = Map.insert name entries acc.rejected }

  pure deduplicated
