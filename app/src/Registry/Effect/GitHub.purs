module Registry.Effect.GitHub where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.DateTime (DateTime)
import Foreign.Git as Git
import Foreign.GitHub (Address, GitHubToken(..), IssueNumber(..), Octokit, RateLimit, Tag, Team)
import Foreign.GitHub as GitHub
import Node.Path as Path
import Registry.Constants as Constants
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))

data GitHub a
  = ListTags Address (Array Tag -> a)
  | ListTeamMembers Team (Array String -> a)
  | GetContent Address String FilePath (String -> a)
  | GetRefCommit Address String (String -> a)
  | GetCommitDate Address String (DateTime -> a)
  | GetRateLimit (RateLimit -> a)

derive instance Functor GitHub

-- | An effect for reading resources from GitHub repositories
type GITHUB r = (github :: GitHub | r)

_github :: Proxy "github"
_github = Proxy

listTags :: forall r. Address -> Run (GITHUB + r) (Array Tag)
listTags address = Run.lift _github (ListTags address identity)

listTeamMembers :: forall r. Team -> Run (GITHUB + r) (Array String)
listTeamMembers team = Run.lift _github (ListTeamMembers team identity)

getContent :: forall r. Address -> String -> FilePath -> Run (GITHUB + r) String
getContent address ref path = Run.lift _github (GetContent address ref path identity)

getRefCommit :: forall r. Address -> String -> Run (GITHUB + r) String
getRefCommit address ref = Run.lift _github (GetRefCommit address ref identity)

getCommitDate :: forall r. Address -> String -> Run (GITHUB + r) DateTime
getCommitDate address ref = Run.lift _github (GetCommitDate address ref identity)

data RegistryRepo a
  = CloseIssue a
  | CommitMetadata PackageName a
  | CommitManifest PackageName Version a
  | CommitPackageSet Version String a

derive instance Functor RegistryRepo

-- | An effect for interacting with Registry-affiliated GitHub repositories,
-- | such as /registry and /registry-index. Requires authentication via the
-- | PACCHETTIBOTTI token.
type REGISTRY_REPO r = (registryRepo :: RegistryRepo | r)

_registryRepo :: Proxy "registryRepo"
_registryRepo = Proxy

-- | Close the issue associated with the running GitHub-triggered event.
closeIssue :: forall r. Run (REGISTRY_REPO + r) Unit
closeIssue = Run.lift _registryRepo (CloseIssue unit)

-- | Commit the metadata file associated with the given package to the registry
-- | repository.
commitMetadata :: forall r. PackageName -> Run (REGISTRY_REPO + r) Unit
commitMetadata name = Run.lift _registryRepo (CommitMetadata name unit)

-- | Commit the manifest file associated with the given package to the registry
-- | index repository.
commitManifest :: forall r. PackageName -> Version -> Run (REGISTRY_REPO + r) Unit
commitManifest name version = Run.lift _registryRepo (CommitManifest name version unit)

-- | Commit the package set associated with the given version to the registry
-- | repository.
commitPackageSet :: forall r. Version -> String -> Run (REGISTRY_REPO + r) Unit
commitPackageSet version message = Run.lift _registryRepo (CommitPackageSet version message unit)

-- | A pure handler for registry repo actions, which are side-effecting and
-- | should be no-ops in a pure environment. Suitable for testing and dry-run
-- | environments.
handleRegistryRepoPure :: forall r a. RegistryRepo a -> Run (LOG + r) a
handleRegistryRepoPure = case _ of
  CloseIssue next -> do
    Log.debug "Not closing issue..."
    pure next
  CommitMetadata name next -> do
    Log.debug $ "Not committing metadata for " <> PackageName.print name <> "..."
    pure next
  CommitManifest name version next -> do
    Log.debug $ "Not committing manifest for " <> formatPackageVersion name version <> "..."
    pure next
  CommitPackageSet version message next -> do
    Log.debug $ "Not committing package set " <> Version.print version <> " with message " <> message
    pure next

type RegistryGitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  , pacchettibotti :: GitHubToken
  , registryPath :: FilePath
  , registryIndexPath :: FilePath
  }

-- | Handle the registry repo effect by closing issues and committing to the
-- | /registry and /registry-index repositories. Requires the Pacchettibotti
-- | auth token.
handleRegistryRepoGitHub :: forall r a. RegistryGitHubEnv -> RegistryRepo a -> Run (AFF + LOG + r) a
handleRegistryRepoGitHub { octokit, issue, pacchettibotti, registryPath, registryIndexPath } = case _ of
  CloseIssue next -> do
    pure next
  {-
  Run.liftAff (Except.runExceptT (GitHub.closeIssue Constants.registry octokit issue)) >>= case _ of
    Left githubError -> do
      Log.error $ "Could not close GitHub issue: " <> GitHub.printGitHubError githubError
      pure next
    Right _ -> do
      Log.debug $ "Closed GitHub issue #" <> show (un IssueNumber issue)
      pure next
  -}

  CommitMetadata name next -> do
    let nameString = PackageName.print name
    let path = Path.concat [ registryPath, Metadata.packageMetadataPath name ]
    Log.debug $ "Committing metadata for " <> nameString <> " to " <> path
    Run.liftAff (commit registryPath path ("Update metadata for " <> nameString)) >>= case _ of
      Left error -> Log.die $ "Failed to commit metadata for " <> nameString <> ":\n" <> error
      Right _ -> pure unit
    let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
    let origin = "https://pacchettibotti:" <> un GitHubToken pacchettibotti <> "@github.com/" <> upstreamRepo <> ".git"
    Run.liftAff (Except.runExceptT (Git.runGitSilent [ "push", origin, "main" ] (Just registryPath))) >>= case _ of
      Left error -> do
        Log.debug error
        Log.die $ "Failed to push metadata for package " <> nameString <> " to " <> upstreamRepo
      Right _ -> do
        Log.info $ "Committed and pushed metadata for " <> nameString
        pure next

  CommitManifest name version next -> do
    let package = formatPackageVersion name version
    let path = Path.concat [ registryIndexPath, ManifestIndex.packageEntryFilePath name ]
    Log.debug $ "Committing manifest for " <> package <> " at path " <> path
    Run.liftAff (commit registryIndexPath path ("Update manifest index entry for " <> package)) >>= case _ of
      Left error -> Log.die $ "Failed to commit manifest index entry for " <> package <> ":\n" <> error
      Right _ -> pure unit
    let upstreamRepo = Constants.packageIndex.owner <> "/" <> Constants.packageIndex.repo
    let origin = "https://pacchettibotti:" <> un GitHubToken pacchettibotti <> "@github.com/" <> upstreamRepo <> ".git"
    Run.liftAff (Except.runExceptT (Git.runGitSilent [ "push", origin, "main" ] (Just registryIndexPath))) >>= case _ of
      Left error -> do
        Log.debug error
        Log.die $ "Failed to push manifest index entry for package " <> package <> " to " <> upstreamRepo
      Right _ -> do
        Log.info $ "Committed and pushed manifest index entry for " <> package <> "!"
        pure next

  CommitPackageSet version message next -> do
    let versionString = Version.print version
    let path = Path.concat [ registryPath, PackageSet.packageSetPath version ]
    Log.debug $ "Committing package set " <> versionString <> " at path " <> path <> " with message:\n" <> message
    Run.liftAff (commit registryPath path message) >>= case _ of
      Left error -> Log.die $ "Failed to commit package set " <> Version.print version <> ":\n" <> error
      Right _ -> pure unit
    let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
    let origin = "https://pacchettibotti:" <> un GitHubToken pacchettibotti <> "@github.com/" <> upstreamRepo <> ".git"
    Run.liftAff (Except.runExceptT (Git.runGitSilent [ "push", origin, "main" ] (Just registryPath))) >>= case _ of
      Left error -> do
        Log.debug error
        Log.die $ "Failed to commit package set " <> versionString <> " to " <> upstreamRepo
      Right _ -> do
        Log.info $ "Committed package set version " <> versionString <> "!"
        pure next
  where
  commit :: FilePath -> FilePath -> String -> _
  commit workdir path message = Except.runExceptT do
    let runGit_ k = Git.runGit_ k (Just workdir)
    runGit_ [ "config", "user.name", "PacchettiBotti" ]
    runGit_ [ "config", "user.email", "<" <> Git.pacchettiBottiEmail <> ">" ]
    runGit_ [ "pull", "--rebase", "--autostash" ]
    runGit_ [ "add", path ]
    runGit_ [ "commit", "-m", message ]

-- -- | Apply exponential backoff to requests that hang, but without cancelling
-- -- | requests if we have reached our rate limit and have been throttled.
-- rateLimitBackoff :: forall a. Octokit -> Aff (Either GitHubError a) -> Aff (Either GitHubError a)
-- rateLimitBackoff octokit action = do
--   maybeResult <- withBackoff
--     { delay: Aff.Milliseconds 5_000.0
--     , action
--     , shouldCancel: \_ -> request octokit getRateLimit >>= case _ of
--         Right { remaining } | remaining == 0 -> pure false
--         _ -> pure true
--     , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
--     }
--   pure $ case maybeResult of
--     Nothing -> Left $ APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
--     Just result -> Right result

-- -- TODO: Migrate into the GITHUB module.
-- --
-- -- | A helper function for implementing GET requests to the GitHub API that
-- -- | relies on the GitHub API to report whether there is any new data, and falls
-- -- | back to the cache if there is not. Should only be used on GET requests; for
-- -- | POST requests, use `request`.
-- cachedRequest
--   :: forall args
--    . (UncachedRequestArgs args -> ExceptT GitHubAPIError Aff Json)
--   -> UncachedRequestArgs args
--   -> { cache :: Cache, checkGitHub :: Boolean }
--   -> ExceptT GitHubAPIError Aff Json
-- cachedRequest runRequest requestArgs@{ route: Route route } { cache, checkGitHub } = do
--   let codec = CA.Common.either githubApiErrorCodec CA.json
--   entry <- liftEffect (Cache.readJsonEntry codec route cache)
--   now <- liftEffect nowUTC
--   ExceptT $ case entry of
--     Left _ -> do
--       Console.log $ "CACHE MISS: Malformed or no entry for " <> route
--       result <- Except.runExceptT $ runRequest requestArgs
--       liftEffect $ Cache.writeJsonEntry codec route result cache
--       pure result

--     Right cached -> case cached.value of
--       Left err
--         -- We don't retry 404 errors because they indicate a missing resource.
--         | err.statusCode == 404 -> pure $ Left err
--         -- Otherwise, if we have an error in cache, we retry the request; we
--         -- don't have anything usable we could return.
--         | otherwise -> do
--             Console.log $ "CACHE ERROR: Deleting non-404 error entry and retrying " <> route
--             Console.logShow err
--             liftEffect $ cache.remove route
--             Except.runExceptT $ cachedRequest runRequest requestArgs { cache, checkGitHub }

--       -- If we do have a usable cache value, then we will defer to GitHub's
--       -- judgment on whether to use it or not. We do that by making a request
--       -- with the 'If-Not-Modified' header. A 304 response means the resource
--       -- has not changed, and GitHub promises not to consume a request if so.
--       --
--       -- Unfortunately, GitHub is not currently (2022-07-01) honoring this
--       -- promise, so we (temporarily) only retry after N hours have passed. Once
--       -- they start honoring the promise again we can remove the modified time
--       -- guard below.
--       --
--       -- TODO: Remove DateTime.diff when GitHub honors requests again.
--       Right payload
--         | checkGitHub, DateTime.diff now cached.modified >= Duration.Hours 1.0 -> do
--             Console.log $ "CACHE EXPIRED: " <> route
--             let _gitHubTime = Formatter.DateTime.format formatRFC1123 cached.modified
--             result <- Except.runExceptT $ runRequest $ requestArgs
--             {- TODO: Re-enable when GitHub honors requests again.
--             result <- Except.runExceptT $ runRequest $ requestArgs
--               { headers = Object.insert "If-Modified-Since" gitHubTime requestArgs.headers }
--             -}
--             case result of
--               -- A 304 response means the resource has not changed and we should
--               -- return from cache.
--               Left err | err.statusCode == 304 -> do
--                 pure $ Right payload
--               _ -> do
--                 liftEffect $ Cache.writeJsonEntry codec route result cache
--                 pure result
--         | otherwise ->
--             pure $ Right payload

-- -- GitHub uses the RFC1123 time format: "Thu, 05 Jul 2022"
-- -- http://www.csgnetwork.com/timerfc1123calc.html
-- --
-- -- It expects the time to be in UTC.
-- formatRFC1123 :: Formatter
-- formatRFC1123 = List.fromFoldable
--   [ DayOfWeekNameShort
--   , Placeholder ", "
--   , DayOfMonthTwoDigits
--   , Placeholder " "
--   , MonthShort
--   , Placeholder " "
--   , YearFull
--   , Placeholder " "
--   , Hours24
--   , Placeholder ":"
--   , MinutesTwoDigits
--   , Placeholder ":"
--   , SecondsTwoDigits
--   , Placeholder " "
--   , Placeholder "UTC"
--   ]
