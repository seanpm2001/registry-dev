module Foreign.GitHub
  ( Address
  , Event(..)
  , GitHubAPIError
  , GitHubError(..)
  , GitHubToken(..)
  , IssueNumber(..)
  , JSArgs
  , Octokit
  , PackageURL(..)
  , RateLimit
  , Request
  , Route
  , Tag
  , Team
  , TeamMember
  , closeIssue
  , createComment
  , decodeEvent
  , getCommitDate
  , getContent
  , getRateLimit
  , getRefCommit
  , githubErrorCodec
  , listTags
  , listTeamMembers
  , mkOctokit
  , parseRepo
  , printGitHubError
  , printRoute
  , request
  , tagCodec
  ) where

import Registry.App.Prelude

import Affjax as Http
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.String.CodeUnits as String.CodeUnits
import Data.Time.Duration as Duration
import Data.Variant as Variant
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object as Object
import Parsing (ParseError)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.Json (Json)
import Registry.App.Json as Json
import Registry.Internal.Codec as Internal.Codec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _
derive newtype instance Eq GitHubToken
derive newtype instance Ord GitHubToken

newtype PackageURL = PackageURL String

derive instance Newtype PackageURL _
derive newtype instance Eq PackageURL
derive newtype instance Ord PackageURL

foreign import data Octokit :: Type

foreign import mkOctokitImpl :: EffectFn1 GitHubToken Octokit

mkOctokit :: GitHubToken -> Effect Octokit
mkOctokit = runEffectFn1 mkOctokitImpl

newtype IssueNumber = IssueNumber Int

instance Newtype IssueNumber Int
derive newtype instance Eq IssueNumber

-- | A team within a GitHub organization
type Team = { org :: String, team :: String }

-- | Member of a GitHub organization
type TeamMember = { username :: String, userId :: Int }

-- | List members of the given team
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/teams/listMembersInOrg.md
listTeamMembers :: Team -> Request (Array TeamMember)
listTeamMembers team =
  { route: Route GET [ "orgs", team.org, "teams", team.team, "members" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginated: true
  , decode: decodeTeamMembers
  }
  where
  decodeTeamMembers :: Json -> Either JsonDecodeError (Array TeamMember)
  decodeTeamMembers = Json.decode (CA.array CA.json) >=> traverse decodeTeamMember

  decodeTeamMember :: Json -> Either JsonDecodeError TeamMember
  decodeTeamMember json = do
    object <- Json.decode CA.jobject json
    username <- Json.atKey "login" CA.string object
    userId <- Json.atKey "id" CA.int object
    pure { username, userId }

-- | List repository tags
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTags :: Address -> Request (Array Tag)
listTags address =
  { route: Route GET [ "repos", address.owner, address.repo, "tags" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginated: true
  , decode: decodeTags
  }
  where
  decodeTags :: Json -> Either JsonDecodeError (Array Tag)
  decodeTags = Json.decode (CA.array CA.json) >=> traverse decodeTag

  decodeTag :: Json -> Either JsonDecodeError Tag
  decodeTag json = do
    object <- Json.decode CA.jobject json
    name <- Json.atKey "name" CA.string object
    commitObject <- Json.atKey "commit" CA.jobject object
    sha <- Json.atKey "sha" CA.string commitObject
    url <- Json.atKey "url" CA.string commitObject
    pure { name, sha, url }

-- | Fetch a specific file  from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/getContent.md
getContent :: Address -> String -> FilePath -> Request String
getContent address ref path =
  { route: Route GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
  , headers: Object.empty
  , args: noArgs
  , paginated: false
  , decode: decodeFile
  }
  where
  decodeFile :: Json -> Either JsonDecodeError String
  decodeFile json = do
    object <- Json.decode CA.jobject json
    data_ <- Json.atKey "data" CA.jobject object
    type_ <- Json.atKey "type" CA.string data_
    encoding <- Json.atKey "encoding" CA.string data_
    if encoding == "base64" && type_ == "file" then do
      contentsb64 <- Json.atKey "content" CA.string data_
      contents <- lmap (\err -> TypeMismatch ("Base64: " <> Exception.message err)) $ traverse Base64.decode $ String.split (String.Pattern "\n") contentsb64
      pure $ fold contents
    else
      Left $ TypeMismatch $ "Base64: " <> show { encoding, type: type_ }

-- | Fetch the commit SHA for a given ref on a GitHub repository
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getRef.md
getRefCommit :: Address -> String -> Request String
getRefCommit address ref = do
  { route: Route GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginated: false
  , decode: decodeRefSha
  }
  where
  decodeRefSha :: Json -> Either JsonDecodeError String
  decodeRefSha json = do
    object <- Json.decode CA.jobject json
    innerObject <- Json.atKey "object" CA.jobject object
    Json.atKey "sha" CA.string innerObject

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Address -> String -> Request DateTime
getCommitDate address commitSha = do
  { route: Route GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginated: true
  , decode: decodeCommit
  }
  where
  decodeCommit :: Json -> Either JsonDecodeError DateTime
  decodeCommit json = do
    object <- Json.decode CA.jobject json
    committerObject <- Json.atKey "committer" CA.jobject object
    Json.atKey "date" Internal.Codec.iso8601DateTime committerObject

-- | Create a comment on an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createComment :: Address -> IssueNumber -> String -> Request Unit
createComment address (IssueNumber issue) body = do
  { route: Route POST [ "repos", address.owner, address.repo, "issues", show issue, "comments" ] Map.empty
  , headers: Object.empty
  , args: toJSArgs { body }
  , paginated: false
  , decode: \_ -> pure unit
  }

-- | Close an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Address -> IssueNumber -> Request Unit
closeIssue address (IssueNumber issue) =
  { route: Route PATCH [ "repos", address.owner, address.repo, "issues", show issue ] Map.empty
  , headers: Object.empty
  , args: toJSArgs { state: "closed" }
  , paginated: false
  , decode: \_ -> pure unit
  }

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

-- | Get the current status of the rate limit imposed by GitHub on their API
getRateLimit :: Request RateLimit
getRateLimit = do
  { route: Route GET [ "rate_limit" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginated: false
  , decode: decodeRateLimit
  }
  where
  decodeRateLimit :: Json -> Either JsonDecodeError RateLimit
  decodeRateLimit json = do
    object <- Json.decode CA.jobject json
    dataObject <- Json.atKey "data" CA.jobject object
    resourcesObject <- Json.atKey "resources" CA.jobject dataObject
    coreObject <- Json.atKey "core" CA.jobject resourcesObject
    limit <- Json.atKey "limit" CA.int coreObject
    remaining <- Json.atKey "remaining" CA.int coreObject
    reset <- Json.atKey "reset" CA.number coreObject
    let resetTime = Instant.instant $ Duration.Milliseconds $ reset * 1000.0
    pure { limit, remaining, resetTime }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
data Route = Route Method (Array String) (Map String String)

derive instance Eq Route
derive instance Ord Route

printRoute :: Route -> String
printRoute (Route method segments params) = show method <> " " <> printPath <> printParams
  where
  printPath = Array.foldMap (append "/") segments
  printParams = case Map.size params of
    0 -> ""
    _ -> append "?" $ String.joinWith "&" $ map (\(Tuple key val) -> key <> "=" <> val) $ Map.toUnfoldable params

-- | An opaque type for PureScript types we want to pass directly to JavaScript
-- | through the FFI.
data JSArgs

toJSArgs :: forall a. Record a -> JSArgs
toJSArgs = unsafeCoerce

noArgs :: JSArgs
noArgs = toJSArgs {}

type Request a =
  { route :: Route
  , headers :: Object String
  , args :: JSArgs
  , paginated :: Boolean
  , decode :: Json -> Either Json.JsonDecodeError a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API.
request :: forall a. Octokit -> Request a -> Aff (Either GitHubError a)
request octokit { route, headers, args, paginated, decode } = do
  result <- Promise.toAffE $ runEffectFn6 (if paginated then paginateImpl else requestImpl) octokit (printRoute route) headers args Left Right
  pure $ case result of
    Left githubError -> case decodeGitHubAPIError githubError of
      Left decodeError -> Left $ UnexpectedError $ Json.printJsonDecodeError decodeError
      Right decoded -> Left $ APIError decoded
    Right json -> case decode json of
      Left decodeError -> Left $ DecodeError $ Json.printJsonDecodeError decodeError
      Right parsed -> Right parsed
  where
  decodeGitHubAPIError :: Object Json -> Either JsonDecodeError GitHubAPIError
  decodeGitHubAPIError object = do
    statusCode <- Json.atKey "status" CA.int object
    message <- case statusCode of
      304 -> pure ""
      _ -> Json.atKey "response" CA.jobject object >>= Json.atKey "data" CA.jobject >>= Json.atKey "message" CA.string
    pure { statusCode, message }

newtype Event = Event
  { issueNumber :: IssueNumber
  , body :: String
  , username :: String
  }

derive instance Newtype Event _

decodeEvent :: Json -> Either JsonDecodeError Event
decodeEvent json = do
  object <- Json.decode CA.jobject json
  username <- Json.atKey "login" CA.string =<< Json.atKey "sender" CA.jobject object

  issueObject <- Json.atKey "issue" CA.jobject object
  issueNumber <- Json.atKey "number" CA.int issueObject

  -- We accept issue creation and issue comment events, but both contain an
  -- 'issue' field. However, only comments contain a 'comment' field. For that
  -- reason we first try to parse the comment and fall back to the issue if
  -- that fails.
  body <- Json.atKey "body" CA.string =<< Json.atKey "comment" CA.jobject object <|> pure issueObject
  pure $ Event { body, username, issueNumber: IssueNumber issueNumber }

type Address = { owner :: String, repo :: String }

type Tag = { name :: String, sha :: String, url :: Http.URL }

tagCodec :: JsonCodec Tag
tagCodec = Json.object "Tag"
  { name: CA.string
  , sha: CA.string
  , url: CA.string
  }

parseRepo :: PackageURL -> Either ParseError Address
parseRepo (PackageURL input) = Parsing.runParser input do
  _ <- Parsing.Combinators.choice
    [ Parsing.String.string "https://github.com/"
    , Parsing.String.string "git://github.com/"
    , Parsing.String.string "git@github.com/"
    ]

  owner <- do
    let
      ownerChoice = Parsing.Combinators.choice
        [ Parsing.String.Basic.alphaNum
        , Parsing.String.char '-'
        ]
    Tuple chars _ <- Parsing.Combinators.Array.manyTill_ ownerChoice (Parsing.String.char '/')
    pure $ String.CodeUnits.fromCharArray chars

  repoWithSuffix <- String.CodeUnits.fromCharArray <$> Array.many Parsing.String.anyChar
  let repo = fromMaybe repoWithSuffix (String.stripSuffix (String.Pattern ".git") repoWithSuffix)

  pure { owner, repo }

data GitHubError
  = UnexpectedError String
  | APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: JsonCodec GitHubError
githubErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { unexpectedError: Right CA.string
  , apiError: Right githubApiErrorCodec
  , decodeError: Right CA.string
  }
  where
  toVariant = case _ of
    UnexpectedError error -> Variant.inj (Proxy :: _ "unexpectedError") error
    APIError error -> Variant.inj (Proxy :: _ "apiError") error
    DecodeError error -> Variant.inj (Proxy :: _ "decodeError") error

  fromVariant = Variant.match
    { unexpectedError: UnexpectedError
    , apiError: APIError
    , decodeError: DecodeError
    }

printGitHubError :: GitHubError -> String
printGitHubError = case _ of
  UnexpectedError message -> Array.fold
    [ "Unexpected error: "
    , message
    ]

  APIError fields -> Array.fold
    [ "GitHub API error ("
    , Int.toStringAs Int.decimal fields.statusCode
    , "): "
    , fields.message
    ]
  DecodeError error -> Array.fold
    [ "Decoding error: "
    , error
    ]

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: JsonCodec GitHubAPIError
githubApiErrorCodec = Json.object "GitHubAPIError"
  { statusCode: CA.int
  , message: CA.string
  }
