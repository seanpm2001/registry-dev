module Foreign.GitHub
  ( Address
  , Base64String(..)
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
  , decodeBase64String
  , decodeEvent
  , formatRFC1123
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
  , routeMethod
  , routePath
  , tagCodec
  ) where

import Registry.App.Prelude

import Affjax as Http
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
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
type TeamMember = { login :: String, id :: Int }

-- | List members of the given team
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/teams/listMembersInOrg.md
listTeamMembers :: Team -> Request (Array TeamMember)
listTeamMembers team =
  { route: Route GET [ "orgs", team.org, "teams", team.team, "members" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec
  }
  where
  codec :: JsonCodec (Array TeamMember)
  codec = CA.array $ Json.object "TeamMember" { login: CA.string, id: CA.int }

-- | List repository tags
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTags :: Address -> Request (Array Tag)
listTags address =
  { route: Route GET [ "repos", address.owner, address.repo, "tags" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: CA.array tagCodec
  }

-- | A newline-delimited base64-encoded file retrieved from the GitHub API
newtype Base64String = Base64String String

derive instance Newtype Base64String _

decodeBase64String :: Base64String -> Either String String
decodeBase64String (Base64String string) =
  case traverse Base64.decode $ String.split (String.Pattern "\n") string of
    Left error -> Left $ Exception.message error
    Right values -> Right $ fold values

-- | Fetch a specific file  from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/getContent.md
getContent :: Address -> String -> FilePath -> Request Base64String
getContent address ref path =
  { route: Route GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: base64FileCodec
  }
  where
  base64FileCodec :: JsonCodec Base64String
  base64FileCodec = Profunctor.dimap toJsonRep fromJsonRep $ Json.object "Content"
    { data: Json.object "Content.data"
        { type: value "file"
        , encoding: value "base64"
        , content: CA.string
        }
    }
    where
    toJsonRep (Base64String str) = { data: { type: "file", encoding: "base64", content: str } }
    fromJsonRep { data: { content } } = Base64String content

    value :: String -> JsonCodec String
    value expected = CA.codec'
      (\json -> CA.decode CA.string json >>= \decoded -> if decoded == expected then pure expected else Left (CA.UnexpectedValue json))
      (\_ -> CA.encode CA.string expected)

-- | Fetch the commit SHA for a given ref on a GitHub repository
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getRef.md
getRefCommit :: Address -> String -> Request String
getRefCommit address ref = do
  { route: Route GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec
  }
  where
  codec :: JsonCodec String
  codec = Profunctor.dimap toJsonRep fromJsonRep $ Json.object "Ref"
    { object: Json.object "Ref.object"
        { sha: CA.string
        }
    }

  toJsonRep sha = { object: { sha } }
  fromJsonRep = _.object.sha

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Address -> String -> Request DateTime
getCommitDate address commitSha = do
  { route: Route GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec
  }
  where
  codec :: JsonCodec DateTime
  codec = Profunctor.dimap toJsonRep fromJsonRep $ Json.object "Commit"
    { committer: Json.object "Commit.committer"
        { date: Internal.Codec.iso8601DateTime
        }
    }

  toJsonRep date = { committer: { date } }
  fromJsonRep = _.committer.date

-- | Create a comment on an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createComment :: Address -> IssueNumber -> String -> Request Unit
createComment address (IssueNumber issue) body = do
  { route: Route POST [ "repos", address.owner, address.repo, "issues", show issue, "comments" ] Map.empty
  , headers: Object.empty
  , args: toJSArgs { body }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (Json.encode CA.null)
  }

-- | Close an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Address -> IssueNumber -> Request Unit
closeIssue address (IssueNumber issue) =
  { route: Route PATCH [ "repos", address.owner, address.repo, "issues", show issue ] Map.empty
  , headers: Object.empty
  , args: toJSArgs { state: "closed" }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (Json.encode CA.null)
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
  , paginate: false
  , codec
  }
  where
  codec :: JsonCodec RateLimit
  codec = Profunctor.dimap toJsonRep fromJsonRep $ Json.object "RateLimit"
    { data: Json.object "RateLimit.data"
        { resources: Json.object "RateLimit.data.resources"
            { core: Json.object "RateLimit.data.resources.core"
                { limit: CA.int
                , remaining: CA.int
                , reset: CA.number
                }
            }
        }
    }

  toJsonRep { limit, remaining, resetTime } = do
    let reset = fromMaybe (-9999.0) ((unwrap <<< Instant.unInstant) <$> resetTime)
    { data: { resources: { core: { limit, remaining, reset } } } }

  fromJsonRep { data: { resources: { core: { limit, remaining, reset } } } } =
    { limit, remaining, resetTime: Instant.instant $ Duration.Milliseconds $ reset * 1000.0 }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
data Route = Route Method (Array String) (Map String String)

derive instance Eq Route
derive instance Ord Route

routeMethod :: Route -> Method
routeMethod (Route method _ _) = method

routePath :: Route -> Array String
routePath (Route _ path _) = path

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
  , paginate :: Boolean
  , codec :: JsonCodec a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API.
request :: forall a. Octokit -> Request a -> Aff (Either GitHubError a)
request octokit { route, headers, args, paginate, codec } = do
  result <- Promise.toAffE $ runEffectFn6 (if paginate then paginateImpl else requestImpl) octokit (printRoute route) headers args Left Right
  pure $ case result of
    Left githubError -> case decodeGitHubAPIError githubError of
      Left decodeError -> Left $ UnexpectedError $ Json.printJsonDecodeError decodeError
      Right decoded -> Left $ APIError decoded
    Right json -> case Json.decode codec json of
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
tagCodec = Profunctor.dimap toJsonRep fromJsonRep $ Json.object "Tag"
  { name: CA.string
  , commit: Json.object "Tag.Commit"
      { sha: CA.string
      , url: CA.string
      }
  }
  where
  toJsonRep { name, sha, url } = { name, commit: { sha, url } }
  fromJsonRep { name, commit } = { name, sha: commit.sha, url: commit.url }

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

-- GitHub uses the RFC1123 time format: "Thu, 05 Jul 2022"
-- http://www.csgnetwork.com/timerfc1123calc.html
--
-- It expects the time to be in UTC.
formatRFC1123 :: Formatter
formatRFC1123 = List.fromFoldable
  [ DayOfWeekNameShort
  , Placeholder ", "
  , DayOfMonthTwoDigits
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , YearFull
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder " "
  , Placeholder "UTC"
  ]
