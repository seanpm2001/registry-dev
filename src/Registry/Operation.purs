module Registry.Operation where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Validation.Semigroup as Validation
import Foreign.FastGlob as FastGlob
import Foreign.SPDX (License)
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Registry.Index (RegistryIndex)
import Registry.Json ((.:))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location, Manifest(..), Metadata, Owner)
import Registry.Solver as Solver
import Registry.Types (Log(..))
import Registry.Version (Version)

type OperationEnv m =
  { log :: Log OperationError -> m Unit
  , registryMetadata :: Map PackageName Metadata
  , registryIndex :: RegistryIndex
  , detectLicense :: FilePath -> License -> m (Either String License)
  , verifyOwner :: Owner -> m (Either String Unit)
  , fetchPackageSource :: m FilePath
  , fetchPackageManifest :: m Manifest
  -- Create the tarball yourself w/ our exported utils
  , fetchTarballPath :: m FilePath
  , matchGlobsSource :: Array String -> m (Array FilePath)
  }

-- Registry will construct its own Env that is correct
-- Need to be abstract enough
-- Is it important to build in spago.yaml files within the Registry?

data OperationError
  = PublishMissingLocation
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
  | PackageSourceFilesOutsideDirectory
  | PackageTarBallSizeExceedsMaximum

-- TODO: printError :: OperationError -> String

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

validatePublish :: forall m. Monad m => MonadAff m => OperationEnv m -> PublishData -> m (Either (Array OperationError) Unit)
validatePublish env payload = runExceptT do
  metadata <- ExceptT $ map (lmap Array.singleton) $ verifyPublishPayload env payload
  manifest <- ExceptT $ map (lmap Array.singleton) $ verifyPursFiles env
  metadataUpdates <- ExceptT $ verifyPackageInfo env payload manifest metadata
  ExceptT $ map (lmap Array.singleton) $ verifyPackageSource env manifest

validateUnpublish :: forall m. Monad m => OperationEnv m -> m Unit
validateUnpublish _ = pure unit

validateTransfer :: forall m. Monad m => OperationEnv m -> m Unit
validateTransfer _ = pure unit

verifyPublishPayload :: forall m. Monad m => OperationEnv m -> PublishData -> m (Either OperationError Metadata)
verifyPublishPayload env { name, location: mbLocation } = runExceptT do
  case Map.lookup name env.registryMetadata of
    Nothing -> case mbLocation of
      Nothing -> throwError PublishMissingLocation
      Just location | not (locationIsUnique location env.registryMetadata) -> throwError PublishLocationNotUnique
      Just location -> pure { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
    Just metadata -> case mbLocation of
      Just location | metadata.location /= location -> throwError UpdateLocationMismatch
      _ -> pure metadata

verifyPursFiles :: forall m. Monad m => MonadAff m => OperationEnv m -> m (Either OperationError Manifest)
verifyPursFiles env = runExceptT do
  packageDirectory <- lift env.fetchPackageSource
  whenM (liftAff $ map (Array.null <<< _.succeeded) $ FastGlob.match packageDirectory [ "src/**/*.purs" ]) do
    throwError MissingPursFiles
  lift env.fetchPackageManifest

-- At each step, need to log & then collect all errors
verifyPackageInfo
  :: forall m
   . Monad m 
  => MonadAff m 
  => OperationEnv m 
  -> PublishData 
  -> Manifest
  -> Metadata
  -> m (Either (Array OperationError) (Metadata -> Metadata))
verifyPackageInfo env publishData manifest metadata = do
  env.log $ Debug "Ensuring Package Name matches manifest"
  let packageNamesMatchResult = packageNamesMatch publishData manifest
  env.log $ Debug "Ensuring Package Name isn't `metadata`"
  let notMetadataPackageResult = notMetadataPackage manifest
  env.log $ Debug "Ensuring package version has not been published or unpublished"
  let versionDoesNotExistResult = versionDoesNotExist manifest metadata
  env.log $ Debug "Ensuring locations match"
  let locationsMatchResult = locationsMatch manifest metadata
  env.log $ Debug "Ensuring owners match"
  let ownersMatchResult = ownersMatch manifest metadata
  env.log $ Debug "Ensuring package dependencies solve"
  let packageDependenciesSolveResult = packageDependenciesSolve manifest env.registryIndex
  env.log $ Debug "Ensuring licenses match"
  licenseMatchResult <- licenseMatch env manifest
  let
    { fail } = partitionEithers
      [ packageNamesMatchResult
      , notMetadataPackageResult
      , versionDoesNotExistResult
      , locationsMatchResult
      , packageDependenciesSolveResult
      , licenseMatchResult
      ]
  case ownersMatchResult of
    Left err -> pure $ Left (err <> join fail)
    Right update -> case join fail of
      [] -> pure $ Right update
      _ -> pure $ Left (join fail)

packageNamesMatch :: PublishData -> Manifest -> Either (Array OperationError) Unit
packageNamesMatch { name: apiName } (Manifest { name: manifestName }) =
  if apiName /= manifestName then
    Left [ PackageNameMismatch ]
  else
    pure unit

notMetadataPackage :: Manifest -> Either (Array OperationError) Unit
notMetadataPackage (Manifest { name }) =
  when (PackageName.print name == "metadata") do
    Left [ MetadataPackageUpload ]

versionDoesNotExist :: Manifest -> Metadata -> Either (Array OperationError) Unit
versionDoesNotExist (Manifest { version }) metadata = Validation.toEither ado
  _ <- case Map.lookup version metadata.published of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyPublished)
    Nothing -> pure unit
  _ <- case Map.lookup version metadata.unpublished of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyUnpublished)
    Nothing -> pure unit
  in unit

locationsMatch :: Manifest -> Metadata -> Either (Array OperationError) Unit
locationsMatch (Manifest { location }) { location: metadataLocation } =
  when (location /= metadataLocation) do
    Left [ LocationMismatch ]

ownersMatch :: Manifest -> Metadata -> Either (Array OperationError) (Metadata -> Metadata)
ownersMatch (Manifest { owners: manifestOwners }) { owners: metadataOwners } =
  if manifestOwners /= metadataOwners then
    pure (_ { owners = manifestOwners })
  else
    pure (\i -> i)

-- If they gave us resolutions, then we skip this step.
packageDependenciesSolve :: Manifest -> RegistryIndex -> Either (Array OperationError) Unit
packageDependenciesSolve (Manifest { dependencies }) registryIndex =
  case Solver.solve (map (map (unwrap >>> _.dependencies)) registryIndex) dependencies of
    Left _ -> Left [ PackageDidNotSolve ]
    Right _ -> pure unit

-- Should already have the package source
licenseMatch :: forall m. Monad m => MonadAff m => OperationEnv m -> Manifest -> m (Either (Array OperationError) Unit)
licenseMatch env (Manifest { license }) = do
  packageSource <- env.fetchPackageSource
  licenseResult <- env.detectLicense packageSource license
  case licenseResult of
    Left _ -> pure $ Left [ LicenseMismatch ]
    Right sourceLicense -> pure do
      when (license /= sourceLicense) do
        Left [ LicenseMismatch ]

verifyPackageSource
  :: forall m
   . Monad m 
  => MonadAff m 
  => OperationEnv m 
  -> Manifest
  -> m (Either OperationError Unit)
verifyPackageSource env (Manifest manifest) = runExceptT do
  tarballPath <- lift env.fetchTarballPath 
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.stat tarballPath
  -- when (bytes > warnPackageBytes) do
  when (bytes > maxPackageBytes) do
    throwError PackageTarBallSizeExceedsMaximum

  case manifest.files of
      Nothing -> pure unit
      Just files -> do
        packageSource <- lift env.fetchPackageSource
        matches <- lift $ env.matchGlobsSource files
        { failed } <- liftAff $ FastGlob.sanitizePaths packageSource matches

        unless (Array.null failed) do
          throwError PackageSourceFilesOutsideDirectory

-- | The absolute maximum bytes allowed in a package
maxPackageBytes :: Number
maxPackageBytes = 2_000_000.0

-- | The number of bytes over which we flag a package for review
warnPackageBytes :: Number
warnPackageBytes = 200_000.0

packageNameIsUnique :: PackageName -> Map PackageName Metadata -> Boolean
packageNameIsUnique name = isNothing <<< Map.lookup name

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location)
