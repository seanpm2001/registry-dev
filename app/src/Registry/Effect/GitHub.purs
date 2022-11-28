module Registry.Effect.GitHub
  ( GITHUB
  , GitHub(..)
  , REGISTRY_REPO
  , RegistryGitHubEnv
  , RegistryRepo(..)
  , _github
  , _registryRepo
  , closeIssue
  , commitManifest
  , commitMetadata
  , commitPackageSet
  , getCommitDate
  , getContent
  , getJsonFile
  , getRefCommit
  , handleGitHubAff
  , handleRegistryRepoGitHub
  , handleRegistryRepoPure
  , listTags
  , listTeamMembers
  ) where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.HTTP.Method (Method(..))
import Data.Time.Duration as Duration
import Foreign.Git as Git
import Foreign.GitHub (Address, GitHubError(..), GitHubToken(..), IssueNumber, Octokit, Request, Tag, Team)
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Object as Object
import Node.Path as Path
import Registry.App.Json (JsonCodec)
import Registry.App.Json as Json
import Registry.App.RegistryCache (RegistryCache(..))
import Registry.App.RegistryCache as App.RegistryCache
import Registry.Constants as Constants
import Registry.Effect.Cache (CACHE)
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
  = ListTags Address (Either GitHubError (Array Tag) -> a)
  | ListTeamMembers Team (Either GitHubError (Array String) -> a)
  | GetContent Address String FilePath (Either GitHubError String -> a)
  | GetRefCommit Address String (Either GitHubError String -> a)
  | GetCommitDate Address String (Either GitHubError DateTime -> a)

derive instance Functor GitHub

-- | An effect for reading resources from GitHub repositories
type GITHUB r = (github :: GitHub | r)

_github :: Proxy "github"
_github = Proxy

listTags :: forall r. Address -> Run (GITHUB + r) (Either GitHubError (Array Tag))
listTags address = Run.lift _github (ListTags address identity)

-- | List the members of the provided team. Requires that the authorization on
-- | the request has read rights for the given organization and team.
listTeamMembers :: forall r. Team -> Run (GITHUB + r) (Either GitHubError (Array String))
listTeamMembers team = Run.lift _github (ListTeamMembers team identity)

getContent :: forall r. Address -> String -> FilePath -> Run (GITHUB + r) (Either GitHubError String)
getContent address ref path = Run.lift _github (GetContent address ref path identity)

getJsonFile :: forall r a. Address -> String -> JsonCodec a -> FilePath -> Run (GITHUB + r) (Either GitHubError a)
getJsonFile address ref codec path = do
  content <- getContent address ref path
  let
    attemptDecode inner = case Json.jsonParser (JsonRepair.tryRepair inner) of
      Left jsonError -> Left $ GitHub.DecodeError $ "Not Json: " <> jsonError
      Right json -> case Json.decode codec json of
        Left decodeError -> Left $ GitHub.DecodeError $ Json.printJsonDecodeError decodeError
        Right decoded -> Right decoded
  pure $ attemptDecode =<< content

getRefCommit :: forall r. Address -> String -> Run (GITHUB + r) (Either GitHubError String)
getRefCommit address ref = Run.lift _github (GetRefCommit address ref identity)

getCommitDate :: forall r. Address -> String -> Run (GITHUB + r) (Either GitHubError DateTime)
getCommitDate address ref = Run.lift _github (GetCommitDate address ref identity)

-- | An effectful handler for the GITHUB effect which makes calls to GitHub
-- | using Octokit.
handleGitHubAff :: forall r a. Octokit -> GitHub a -> Run (CACHE RegistryCache + LOG + AFF + r) a
handleGitHubAff octokit = case _ of
  ListTags address reply -> do
    Log.debug $ "Listing tags for " <> show address
    result <- request octokit (GitHub.listTags address)
    pure $ reply result

  ListTeamMembers team reply -> do
    Log.debug $ "Listing members of team " <> show team
    result <- request octokit (GitHub.listTeamMembers team)
    pure $ reply $ map (map _.login) result

  GetContent address ref path reply -> do
    Log.debug $ "Fetching content from " <> show address <> " at ref " <> ref <> " at path " <> path
    request octokit (GitHub.getContent address ref path) >>= case _ of
      Left error -> pure $ reply $ Left error
      Right result -> case GitHub.decodeBase64String result of
        Left base64Error -> do
          Log.debug $ "Failed to decode base64-encoded file: " <> base64Error
          pure $ reply $ Left $ DecodeError base64Error
        Right decoded ->
          pure $ reply $ Right decoded

  GetRefCommit address ref reply -> do
    Log.debug $ "Fetching commit associated with ref " <> ref <> " on repository " <> show address
    result <- request octokit (GitHub.getRefCommit address ref)
    pure $ reply result

  GetCommitDate address ref reply -> do
    Log.debug $ "Fetching commit date associated with ref " <> ref <> " on repository " <> show address
    result <- request octokit (GitHub.getCommitDate address ref)
    pure $ reply result

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
handleRegistryRepoGitHub :: forall r a. RegistryGitHubEnv -> RegistryRepo a -> Run (CACHE RegistryCache + AFF + LOG + r) a
handleRegistryRepoGitHub { octokit, issue, pacchettibotti, registryPath, registryIndexPath } = case _ of
  CloseIssue next -> do
    request octokit (GitHub.closeIssue Constants.registry issue) >>= case _ of
      Left githubError -> do
        Log.error $ "Could not close GitHub issue: " <> GitHub.printGitHubError githubError
        pure next
      Right _ -> do
        Log.debug $ "Closed GitHub issue #" <> show (un GitHub.IssueNumber issue)
        pure next

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

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
requestWithBackoff :: forall a r. Octokit -> Request a -> Run (LOG + AFF + r) (Either GitHubError a)
requestWithBackoff octokit githubRequest = do
  Log.debug $ "Making request to " <> GitHub.printRoute githubRequest.route
  let action = GitHub.request octokit githubRequest
  result <- Run.liftAff $ withBackoff
    { delay: Duration.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> GitHub.request octokit GitHub.getRateLimit >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case result of
    Nothing -> pure $ Left $ GitHub.APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just accepted -> pure accepted

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
request :: forall r a. GitHub.Octokit -> GitHub.Request a -> Run (CACHE RegistryCache + LOG + AFF + r) (Either GitHubError a)
request octokit githubRequest@{ route, codec } = do
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case GitHub.routeMethod route of
    GET | route /= GitHub.getRateLimit.route -> do
      entry <- App.RegistryCache.get (GitHubRequest route)
      now <- Run.liftAff $ liftEffect nowUTC
      case entry of
        Nothing -> do
          Log.debug $ "No cache entry for route " <> GitHub.printRoute route
          result <- requestWithBackoff octokit githubRequest
          App.RegistryCache.put (GitHubRequest route) (map (Json.encode codec) result)
          pure result

        Just cached -> case cached.value of
          Left (GitHub.APIError err)
            -- We don't retry 404 errors because they indicate a missing resource.
            | err.statusCode == 404 -> do
                Log.debug "Cached entry is a 404 error, not retrying..."
                pure $ Left $ GitHub.APIError err
            -- Otherwise, if we have an error in cache, we retry the request; we
            -- don't have anything usable we could return.
            | otherwise -> do
                Log.debug $ "Retrying route " <> GitHub.printRoute route <> " because cache contains non-404 error: " <> show err
                App.RegistryCache.delete (GitHubRequest route)
                request octokit githubRequest

          Left otherError -> do
            Log.debug "Cached entry is an unknown or decode error, not retrying..."
            pure (Left otherError)

          -- If we do have a usable cache value, then we will defer to GitHub's
          -- judgment on whether to use it or not. We do that by making a request
          -- with the 'If-Not-Modified' header. A 304 response means the resource
          -- has not changed, and GitHub promises not to consume a request if so.
          --
          -- Unfortunately, GitHub is not currently (2022-07-01) honoring this
          -- promise, so we (temporarily) only retry after N hours have passed. Once
          -- they start honoring the promise again we can remove the modified time
          -- guard below.
          --
          -- TODO: Remove DateTime.diff when GitHub honors requests again.
          Right _ | DateTime.diff now cached.modified >= Duration.Hours 1.0 -> do
            Log.debug $ "Cache entry expired for route " <> GitHub.printRoute route <> ", requesting..."
            -- This is how we *would* modify the request, once GitHub works.
            let _githubTime = Formatter.DateTime.format GitHub.formatRFC1123 cached.modified
            let _modifiedRequest = githubRequest { headers = Object.insert "If-Modified-Since" _githubTime githubRequest.headers }
            result <- requestWithBackoff octokit githubRequest
            case result of
              Left (GitHub.APIError err) | err.statusCode == 304 -> do
                Log.debug $ "Received confirmation of cache validity response from GitHub, reading cache value..."
                pure result
              _ -> do
                App.RegistryCache.put (GitHubRequest route) (map (Json.encode codec) result)
                pure result

          Right value -> case Json.decode codec value of
            Left error -> do
              Log.debug $ "Unable to decode cache entry, returning error..."
              pure $ Left $ GitHub.DecodeError $ Json.printJsonDecodeError error
            Right accepted ->
              pure $ Right accepted

    _ -> do
      Log.debug $ "Not a cacheable route: " <> GitHub.printRoute route <> ", requesting..."
      requestWithBackoff octokit githubRequest
