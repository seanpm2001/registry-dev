module Registry.App.API2 where

import Registry.App.Prelude hiding ((/))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..), IssueNumber(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import HTTPurple (class Generic, Method(..), Request, Response, RouteDuplex', ServerM, (/))
import HTTPurple as HTTPurple
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.RegistryM (RegistryM)
import Registry.App.RegistryM as RegistryM
import Registry.Operation as Operation

-- THOUGHTS
--
-- We really have two execution environments: GitHub and the HTTP API. We should
-- really have two variants on RegistryM to handle these two environments, too.
-- I'm sympathetic to using Run and having a GitHub interpreter and a server
-- interpreter.

main :: ServerM
main = HTTPurple.serve { port: 8080, onStarted } { route, router: registryMiddleware router }
  where
  onStarted :: Effect Unit
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080/v1/publish  │"
    log " └───────────────────────────────────────┘"

data Route
  = Publish
  | Unpublish
  | Transfer

derive instance Generic Route _

-- TODO: When we support webhooks for Spago, Spago will need to send a webhook
-- URL along with the request. This may need to be a separate route so tha users
-- can decide to receive errors from the request directly or to receive webhook
-- events instead.
route :: RouteDuplex' Route
route = HTTPurple.root $ HTTPurple.prefix "api" $ HTTPurple.prefix "v1" $ HTTPurple.sum
  { "Publish": "publish" / HTTPurple.noArgs
  , "Unpublish": "unpublish" / HTTPurple.noArgs
  , "Transfer": "transfer" / HTTPurple.noArgs
  }

registryMiddleware :: (Request Route -> RegistryM Response) -> Request Route -> Aff Response
registryMiddleware router' request' = do
  FS.Extra.ensureDirectory API.scratchDir
  cache <- Cache.useCache API.cacheDir
  packagesMetadata <- liftEffect $ Ref.new Map.empty
  octokit <- liftEffect $ GitHub.mkOctokit $ GitHubToken ""
  RegistryM.runRegistryM (API.mkEnv octokit cache packagesMetadata (IssueNumber (-1)) mempty) do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef
    router' request'

router :: Request Route -> RegistryM Response
router request = case request.route of
  Publish | request.method == Post -> HTTPurple.usingCont do
    body <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) request.body
    -- TODO: This should really be a fork acknowledging receipt but not actualy
    -- processing, once we validate the operation is OK.
    lift $ API.runOperation API.API (Right $ Operation.Publish body)
    HTTPurple.ok "received publish operation"

  Unpublish | request.method == Post -> HTTPurple.usingCont do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) request.body
    case auth.payload of
      Operation.Unpublish _ -> do
        lift $ API.runOperation API.API (Right $ Operation.Authenticated auth)
        HTTPurple.ok "received unpublish operation"
      _ ->
        HTTPurple.badRequest "expected unpublish operation"

  Transfer | request.method == Post -> HTTPurple.usingCont do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) request.body
    case auth.payload of
      Operation.Transfer _ -> do
        lift $ API.runOperation API.API (Right $ Operation.Authenticated auth)
        HTTPurple.ok "received transfer operation"
      _ ->
        HTTPurple.badRequest "expected transfer operation"

  _ ->
    HTTPurple.notFound

jsonDecoder :: forall a. CA.JsonCodec a -> HTTPurple.JsonDecoder CA.JsonDecodeError a
jsonDecoder codec = HTTPurple.JsonDecoder (CA.decode codec <=< parseJson)
  where
  parseJson :: String -> Either CA.JsonDecodeError Json
  parseJson = lmap (\_ -> CA.TypeMismatch "JSON") <<< Argonaut.Parser.jsonParser

jsonEncoder :: forall a. CA.JsonCodec a -> HTTPurple.JsonEncoder a
jsonEncoder codec = HTTPurple.JsonEncoder (Argonaut.Core.stringify <<< CA.encode codec)
