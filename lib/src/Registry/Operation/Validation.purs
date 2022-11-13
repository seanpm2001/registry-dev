module Registry.Operation.Validation where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (isLeft)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NES
import Data.Validation.Semigroup as Validation
import Foreign.FastGlob as FastGlob
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Registry.Internal.Path as Internal.Path
import Registry.License (License)
import Registry.Location (Location)
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata(..))
import Registry.Operation (PublishData)
import Registry.Owner (Owner)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Solver as Solver
import Type.Row (type (+))

type OperationEnv m =
  { onStart :: PublishChecksStarted
  , onError :: {}
  , onSuccess :: {}
  , registryMetadata :: Map PackageName Metadata
  , registryIndex :: ManifestIndex
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

-- TODO:
-- - For each OperationError, add context to constructor
-- - Add printOperationError function
-- - Do I need to handle whether or not they pass in a build plan?
-- - 
-- - 
-- - 

type LocationExists r m a = ( locationExists :: r -> m Unit | a )
type LocationUnique r m a = ( locationUnique :: r -> m Unit | a )
type UpdateLocationMatches r m a = ( updateLocationMatches :: r -> m Unit | a )
type PursFilesExist r m a = ( pursFilesExist :: r -> m Unit | a )

type PublishChecksNotification r m =
  Record
    ( LocationExists r m
    + LocationUnique r m
    + UpdateLocationMatches r m
    + PursFilesExist r m 
    + ()
    )

type PublishChecksStarted m = PublishChecksNotification Unit m

type PublishChecksSucceeded m = PublishChecksNotification Unit m

type PublishChecksFailed m =
  Record
    ( LocationExists Unit m
    + LocationUnique (Tuple PackageName Metadata) m
    + UpdateLocationMatches Unit m
    + PursFilesExist Unit m 
    + ()
    )

--displayStarted ::
--displaySucceeded ::
--display ::

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
  | PackageTarballSizeExceedsMaximum Number

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
      Just location -> pure $ Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
    Just metadata -> case mbLocation of
      Just location | (unwrap metadata).location /= location -> throwError UpdateLocationMismatch
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
  --env.log $ Debug "Ensuring Package Name matches manifest"
  let packageNamesMatchResult = packageNamesMatch publishData manifest
  --env.log $ Debug "Ensuring Package Name isn't `metadata`"
  let notMetadataPackageResult = notMetadataPackage manifest
  --env.log $ Debug "Ensuring package version has not been published or unpublished"
  let versionDoesNotExistResult = versionDoesNotExist manifest metadata
  --env.log $ Debug "Ensuring locations match"
  let locationsMatchResult = locationsMatch manifest metadata
  --env.log $ Debug "Ensuring owners match"
  let ownersMatchResult = ownersMatch manifest metadata
  --env.log $ Debug "Ensuring package dependencies solve"
  let packageDependenciesSolveResult = packageDependenciesSolve manifest env.registryIndex
  --env.log $ Debug "Ensuring licenses match"
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
versionDoesNotExist (Manifest { version }) (Metadata metadata) = Validation.toEither ado
  _ <- case Map.lookup version metadata.published of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyPublished)
    Nothing -> pure unit
  _ <- case Map.lookup version metadata.unpublished of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyUnpublished)
    Nothing -> pure unit
  in unit

locationsMatch :: Manifest -> Metadata -> Either (Array OperationError) Unit
locationsMatch (Manifest { location }) (Metadata { location: metadataLocation }) =
  when (location /= metadataLocation) do
    Left [ LocationMismatch ]

ownersMatch :: Manifest -> Metadata -> Either (Array OperationError) (Metadata -> Metadata)
ownersMatch (Manifest { owners: manifestOwners }) (Metadata { owners: metadataOwners }) =
  if manifestOwners /= metadataOwners then
    pure (unwrap >>> _ { owners = manifestOwners } >>> Metadata)
  else
    pure (\i -> i)

-- If they gave us resolutions, then we skip this step.
packageDependenciesSolve :: Manifest -> ManifestIndex -> Either (Array OperationError) Unit
packageDependenciesSolve (Manifest { dependencies }) manifestIndex =
  case Solver.solve ((map (map (unwrap >>> _.dependencies)) (ManifestIndex.toMap manifestIndex))) dependencies of
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

  --when (bytes > warnPackageBytes) do
    --lift $ env.log $ Warn $ "Package tarball is " <> show bytes <> ".\ncc: @purescript/packaging"
  when (bytes > maxPackageBytes) do
    throwError $ PackageTarballSizeExceedsMaximum bytes

  case manifest.files of
      Nothing -> pure unit
      Just files -> do
        packageSource <- lift env.fetchPackageSource
        matches <- lift $ env.matchGlobsSource (NEA.toArray (map NES.toString files))
        results <- liftAff $ traverse (Internal.Path.sanitizePath packageSource) matches

        unless (Array.null (Array.filter isLeft results)) do
          throwError PackageSourceFilesOutsideDirectory

-- | The absolute maximum bytes allowed in a package
maxPackageBytes :: Number
maxPackageBytes = 2_000_000.0

-- | The number of bytes over which we flag a package for review
warnPackageBytes :: Number
warnPackageBytes = 200_000.0

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location <<< unwrap)
