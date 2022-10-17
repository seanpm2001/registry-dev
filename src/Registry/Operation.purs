module Registry.Operation where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Map as Map
import Data.Validation.Semigroup as Validation
import Foreign.FastGlob as FastGlob
import Foreign.SPDX (License)
import Foreign.Tmp as Tmp
import Registry.Index (RegistryIndex)
import Registry.Json ((.:))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location, Manifest(..), Metadata, Owner)
import Registry.Solver as Solver
import Registry.Types (Log)
import Registry.Version (Version)

type OperationEnv m =
  { log :: Log OperationError -> m Unit
  , registryMetadata :: Map PackageName Metadata
  , registryIndex :: RegistryIndex
  , detectLicense :: FilePath -> License -> m (Either String License)
  , verifyOwner :: Owner -> m (Either String Unit)
  , fetchPackageSource :: m FilePath
  -- If there is a spago-yaml library, should this take a `m FilePath` and use code to do `String -> Maybe SpagoYAML` & we export a `SpagoYAML -> Manifest`
  , fetchPackageManifest :: m Manifest
  }

-- Registry will construct its own Env that is correct
-- Need to be abstract enough
-- Is it important to build in spago.yaml files within the Registry?

data OperationError
  = OperationError
  | PublishMissingLocation
  | PublishLocationNotUnique
  | UpdateLocationMismatch
  | MissingPursFiles
  | PackageNameMismatch
  | MetadataPackageUpload
  | VersionAlreadyPublished
  | VersionAlreadyUnpublished
  | LocationMismatch
  | PackageDidNotSolve
  | LicenseMismatch

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

validatePublish :: forall m. Monad m => MonadAff m => OperationEnv m -> PublishData -> m Unit
validatePublish env payload = do
  _ <- verifyPublishPayload env payload
  manifest <- verifyPackageSource env
  pure unit

validateUnpublish :: forall m. Monad m => OperationEnv m -> m Unit
validateUnpublish _ = pure unit

validateTransfer :: forall m. Monad m => OperationEnv m -> m Unit
validateTransfer _ = pure unit

verifyPublishPayload :: forall m. Monad m => OperationEnv m -> PublishData -> m (Either OperationError Unit)
verifyPublishPayload env { name, location } = runExceptT do
  case Map.lookup name env.registryMetadata of
    Nothing -> case location of
      Nothing -> throwError PublishMissingLocation
      Just l | not (locationIsUnique l env.registryMetadata) -> throwError PublishLocationNotUnique
      Just _ -> pure unit
    Just m -> case location of
      Just l | m.location /= l -> throwError UpdateLocationMismatch
      Just _ -> pure unit
      Nothing -> pure unit

verifyPackageSource :: forall m. Monad m => MonadAff m => OperationEnv m -> m (Either OperationError Manifest)
verifyPackageSource env = runExceptT do
  packageDirectory <- lift env.fetchPackageSource
  whenM (liftAff $ map (Array.null <<< _.succeeded) $ FastGlob.match packageDirectory [ "src/**/*.purs" ]) do
    throwError MissingPursFiles
  -- TODO: If there happened to be multiple, how do we ensure that purs.json is used?
  lift env.fetchPackageManifest

verifyPackageInfo
  :: forall m
   . Monad m 
  => MonadAff m 
  => OperationEnv m 
  -> Map PackageName Metadata
  -> PublishData 
  -> Manifest 
  -> Either (Array OperationError) Unit
verifyPackageInfo env metadata publishData manifestData =
  pure unit

packageNamesMatch :: PublishData -> Manifest -> Validation.V (Array OperationError) Unit
packageNamesMatch { name: apiName } (Manifest { name: manifestName }) =
  if apiName /= manifestName then
    Validation.invalid (Array.singleton PackageNameMismatch)
  else
    pure unit

notMetadataPackage :: Manifest -> Validation.V (Array OperationError) (Metadata -> Metadata)
notMetadataPackage (Manifest { name }) =
  if PackageName.print name == "metadata" then
    Validation.invalid (Array.singleton MetadataPackageUpload)
  else
    pure identity

versionDoesNotExist :: Manifest -> Metadata -> Validation.V (Array OperationError) (Metadata -> Metadata)
versionDoesNotExist (Manifest { name, version }) metadata = ado
  _ <- case Map.lookup version metadata.published of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyPublished)
    Nothing -> pure identity
  _ <- case Map.lookup version metadata.unpublished of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyUnpublished)
    Nothing -> pure identity
  in identity

-- We already validated the location in PublishData, we should thread it through here
locationsMatch :: PublishData -> Manifest -> Validation.V (Array OperationError) (Metadata -> Metadata)
locationsMatch { location: apiLocation } (Manifest { location: manifestLocation }) =
  case apiLocation of
    Just location ->
      if location /= manifestLocation then
        Validation.invalid (Array.singleton LocationMismatch)
      else
        pure identity
    Nothing -> pure identity

-- We've already read the metadata a few times - thread it through
-- This is only for updates - publish is already correct
ownersMatch :: Manifest -> Metadata -> Validation.V (Array OperationError) (Metadata -> Metadata)
ownersMatch (Manifest { owners: manifestOwners }) { owners: metadataOwners } =
  if manifestOwners /= metadataOwners then
    pure (_ { owners = manifestOwners })
  else
    pure unit

-- If they gave us resolutions, then we skip this step.
packageDependenciesSolve :: Manifest -> RegistryIndex -> Validation.V (Array OperationError) (Metadata -> Metadata)
packageDependenciesSolve (Manifest { dependencies }) registryIndex =
  case Solver.solve registryIndex dependencies of
    Left _ -> Validation.invalid (Array.singleton PackageDidNotSolve)
    Right _ -> pure identity

-- Should already have the package source
licenseMatch :: forall m. Monad m => MonadAff m => OperationEnv m -> FilePath -> Manifest -> m (Validation.V (Array OperationError) (Metadata -> Metadata))
licenseMatch env packageSource (Manifest { license }) = do
  licenseResult <- env.detectLicense license packageSource
  case licenseResult of
    Left _ -> Validation.invalid (Array.singleton LicenseMismatch)
    Right sourceLicense ->
      if license /= sourceLicense then
        Validation.invalid (Array.singleton LicenseMismatch)
      else
        pure identity

verifyPackageSource :: forall m. Monad m => MonadAff m => OperationEnv m -> FilePath -> m (Either OperationError Unit)
verifyPackageSource packageSource = do
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  FSE.copy { from: packageSource, to: tmpDir, preserveTimestamps: true }
  pure unit

packageNameIsUnique :: PackageName -> Map PackageName Metadata -> Boolean
packageNameIsUnique name = isNothing <<< Map.lookup name

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location)

