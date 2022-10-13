module Registry.Operation where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Data.Map as Map
import Foreign.SPDX (License)
import Registry.Index (RegistryIndex)
import Registry.Json ((.:))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location, Metadata, Owner)
import Registry.Types (Log)
import Registry.Version (Version)

type OperationEnv m =
  { log :: Log OperationError -> m Unit
  , registryMetadata :: Map PackageName Metadata
  , registryIndex :: RegistryIndex
  , detectLicense :: FilePath -> m (Either String License)
  , verifyOwner :: Owner -> m (Either String Unit)
  }

data OperationError
  = OperationError
  | PublishMissingLocation
  | PublishLocationNotUnique
  | UpdateLocationMismatch

data Operation
  = Publish PublishData
  | PackageSetUpdate PackageSetUpdateData
  | Authenticated AuthenticatedData

derive instance Eq Operation

instance Show Operation where
  show = case _ of
    Publish inner -> "Publish (" <> show (showWithPackage inner) <> ")"
    PackageSetUpdate inner -> "PackageSetUpdate (" <> show inner <> ")"
    Authenticated inner -> "Authenticated (" <> show inner <> ")"
    where
    showWithPackage :: forall r. { name :: PackageName | r } -> { name :: String | r }
    showWithPackage inner =
      inner { name = "PackageName (" <> PackageName.print inner.name <> ")" }

instance RegistryJson Operation where
  encode = case _ of
    Publish fields -> Json.encode fields
    PackageSetUpdate fields -> Json.encode fields
    Authenticated fields -> Json.encode fields

  decode json = do
    let parsePublish = Publish <$> Json.decode json
    let parsePackageSetUpdate = PackageSetUpdate <$> Json.decode json
    let parseAuthenticated = Authenticated <$> Json.decode json
    parsePublish
      <|> parsePackageSetUpdate
      <|> parseAuthenticated

data AuthenticatedOperation
  = Unpublish UnpublishData
  | Transfer TransferData

derive instance Eq AuthenticatedOperation

instance RegistryJson AuthenticatedOperation where
  encode = case _ of
    Unpublish fields -> Json.encode fields
    Transfer fields -> Json.encode fields

  decode json = do
    let parseUnpublish = Unpublish <$> Json.decode json
    let parseTransfer = Transfer <$> Json.decode json
    parseUnpublish <|> parseTransfer

instance Show AuthenticatedOperation where
  show = case _ of
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
    Transfer inner -> "Transfer (" <> show (showWithPackage inner) <> ")"
    where
    showWithPackage :: forall r. { name :: PackageName | r } -> { name :: String | r }
    showWithPackage inner =
      inner { name = "PackageName (" <> PackageName.print inner.name <> ")" }

newtype AuthenticatedData = AuthenticatedData
  { payload :: AuthenticatedOperation
  -- We include the unparsed payload for use in verification so as to preserve
  -- any quirks of formatting that could change the input.
  , rawPayload :: String
  , signature :: Array String
  , email :: String
  }

derive instance Newtype AuthenticatedData _
derive newtype instance Eq AuthenticatedData
derive newtype instance Show AuthenticatedData

instance RegistryJson AuthenticatedData where
  encode (AuthenticatedData fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    rawPayload <- obj .: "payload"
    payload <- Json.parseJson rawPayload
    signature <- obj .: "signature"
    email <- obj .: "email"
    pure $ AuthenticatedData { rawPayload, payload, signature, email }

type PublishData =
  { name :: PackageName
  , location :: Maybe Location
  , ref :: String
  , compiler :: Version
  , resolutions :: Maybe (Map PackageName Version)
  }

type UnpublishData =
  { name :: PackageName
  , version :: Version
  , reason :: String
  }

type TransferData =
  { name :: PackageName
  , newLocation :: Location
  }

type PackageSetUpdateData =
  { compiler :: Maybe Version
  , packages :: Map PackageName (Maybe Version)
  }

validatePublish :: forall m. Monad m => OperationEnv m -> PublishData -> m Unit
validatePublish env payload = do
  publishResult <- verifyPublishPayload env payload
  pure unit

validateUnpublish :: forall m. Monad m => OperationEnv m -> m Unit
validateUnpublish _ = pure unit

validateTransfer :: forall m. Monad m => OperationEnv m -> m Unit
validateTransfer _ = pure unit

verifyPublishPayload :: forall m. Monad m => OperationEnv m -> PublishData -> m (Either OperationError Unit)
verifyPublishPayload env { name, location } = runExceptT do
  case Map.lookup name env.registryMetadata of
    Nothing ->  case location of
      Nothing -> throwError PublishMissingLocation
      Just l | not (locationIsUnique l env.registryMetadata) -> throwError PublishLocationNotUnique
      Just _ -> pure unit
    Just m -> case location of
      Just l | m.location /= l -> throwError UpdateLocationMismatch
      Just _ -> pure unit
      Nothing -> pure unit

packageNameIsUnique :: PackageName -> Map PackageName Metadata -> Boolean
packageNameIsUnique name = isNothing <<< Map.lookup name

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location)

