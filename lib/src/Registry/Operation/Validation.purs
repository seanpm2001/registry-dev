module Registry.Operation.Validation where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (isLeft)
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NES
import Data.Validation.Semigroup as Validation
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Ref as Ref
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
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type OperationEnv m =
  { handlers :: CheckHandlers
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

_locationExists = Proxy :: Proxy "locationExists"
_locationUnique = Proxy :: Proxy "locationUnique"
_updateLocationMatches = Proxy :: Proxy "updateLocationMatches"
_pursFilesExist = Proxy :: Proxy "pursFilesExist"

locationExists :: forall a v. a -> Variant (locationExists :: a | v)
locationExists = Variant.inj _locationExists

locationUnique :: forall a v. a -> Variant (locationUnique :: a | v)
locationUnique = Variant.inj _locationUnique

updateLocationMatches :: forall a v. a -> Variant (updateLocationMatches :: a | v)
updateLocationMatches = Variant.inj _updateLocationMatches

type CheckNotification =
  ( locationExists :: Unit
  , locationUnique :: Unit
  , updateLocationMatches :: Unit
  , pursFilesExist :: Unit
  )

type CheckFailure =
  ( locationExists :: Unit
  , locationUnique :: { packageName :: PackageName, location :: Location }
  , updateLocationMatches :: { old :: Location, new :: Location }
  , pursFilesExist :: Unit
  ) 

type CheckNotificationHandler m =
  { locationExists :: Unit -> m Unit
  , locationUnique :: Unit -> m Unit
  , updateLocationMatches :: Unit -> m Unit
  , pursFilesExist :: Unit -> m Unit
  }

onStart :: forall m. MonadEffect m => OperationEnv m -> Variant CheckNotification -> m Unit
onStart env =
  Variant.case_
    # Variant.on _locationExists (\reason -> liftEffect (foldMap (Ref.write (Just (Started reason))) env.handlers.locationExists))
    # Variant.on _locationUnique (\reason -> liftEffect (foldMap (Ref.write (Just (Started reason))) env.handlers.locationUnique))
    # Variant.on _updateLocationMatches (\reason -> liftEffect (foldMap (Ref.write (Just (Started reason))) env.handlers.updateLocationMatches))
    # Variant.on _pursFilesExist (\reason -> liftEffect (foldMap (Ref.write (Just (Started reason))) env.handlers.pursFilesExist))

onSuccess :: forall m. MonadEffect m => OperationEnv m -> Variant CheckNotification -> m Unit
onSuccess env =
  Variant.case_
    # Variant.on _locationExists (\reason -> liftEffect (foldMap (Ref.write (Just (Finished reason))) env.handlers.locationExists))
    # Variant.on _locationUnique (\reason -> liftEffect (foldMap (Ref.write (Just (Finished reason))) env.handlers.locationUnique))
    # Variant.on _updateLocationMatches (\reason -> liftEffect (foldMap (Ref.write (Just (Finished reason))) env.handlers.updateLocationMatches))
    # Variant.on _pursFilesExist (\reason -> liftEffect (foldMap (Ref.write (Just (Finished reason))) env.handlers.pursFilesExist))

onError :: forall m. MonadEffect m => OperationEnv m -> Variant CheckFailure -> m Unit
onError env =
  Variant.case_
    # Variant.on _locationExists (\reason -> liftEffect (foldMap (Ref.write (Just (Failed reason))) env.handlers.locationExists))
    # Variant.on _locationUnique (\reason -> liftEffect (foldMap (Ref.write (Just (Failed reason))) env.handlers.locationUnique))
    # Variant.on _updateLocationMatches (\reason -> liftEffect (foldMap (Ref.write (Just (Failed reason))) env.handlers.updateLocationMatches))
    # Variant.on _pursFilesExist (\reason -> liftEffect (foldMap (Ref.write (Just (Failed reason))) env.handlers.pursFilesExist))

-- TODO: Are started & finished always Unit?
data CheckStatus started finished failed
  = Started started
  | Finished finished
  | Failed failed

type CheckHandlers =
  { locationExists :: Maybe (Ref (Maybe (CheckStatus Unit Unit Unit)))
  , locationUnique :: Maybe (Ref (Maybe (CheckStatus Unit Unit { packageName :: PackageName, location :: Location })))
  , updateLocationMatches :: Maybe (Ref (Maybe (CheckStatus Unit Unit { old :: Location, new :: Location })))
  , pursFilesExist :: Maybe (Ref (Maybe (CheckStatus Unit Unit Unit)))
  }

noopCheckHandlers :: CheckHandlers
noopCheckHandlers =
  { locationExists: Nothing
  , locationUnique: Nothing
  , updateLocationMatches: Nothing
  , pursFilesExist: Nothing
  }

displayStarted :: Variant CheckNotification -> String
displayStarted = 
  Variant.case_
    # Variant.on _locationExists (\_ -> "Checking that location exists in PublishData...")
    # Variant.on _locationUnique (\_ -> "")
    # Variant.on _updateLocationMatches (\_ -> "")
    # Variant.on _pursFilesExist (\_ -> "")

displayFailure :: Variant CheckFailure -> String
displayFailure =
  Variant.case_
    # Variant.on _locationExists (\_ -> "Location doesn't exist in PublishData.")
    # Variant.on _locationUnique (\{ packageName } -> "Location already taken by " <> PackageName.print packageName <> ".")
    # Variant.on _updateLocationMatches (\_ -> "")
    # Variant.on _pursFilesExist (\_ -> "")

data OperationError
  = PublishMissingLocation
  | PublishLocationNotUnique { packageName :: PackageName, location :: Location }
  | UpdateLocationMismatch { old :: Location, new :: Location }
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

verifyPublishPayload :: forall m. MonadEffect m => OperationEnv m -> PublishData -> m (Either OperationError Metadata)
verifyPublishPayload env { name, location: mbLocation } = runExceptT do
  lift $ onStart env (locationExists unit)
  case Map.lookup name env.registryMetadata of
    Nothing -> case mbLocation of
      Nothing -> do
        lift $ onError env (locationExists unit)
        throwError PublishMissingLocation
      Just location -> do
        lift $ onSuccess env (locationExists unit)
        lift $ onStart env (locationUnique unit)
        case locationIsUnique location env.registryMetadata of
          Just packageName -> do
            lift $ onError env (locationUnique { packageName, location })
            throwError (PublishLocationNotUnique { packageName, location })
          Nothing -> do
            lift $ onSuccess env (locationUnique unit)
            pure $ Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
    Just metadata -> do
      lift $ onSuccess env (locationExists unit)
      lift $ onStart env (updateLocationMatches unit)
      case mbLocation of
        Just location | (unwrap metadata).location /= location -> do
          lift $ onError env (updateLocationMatches { old: (unwrap metadata).location, new: location })
          throwError (UpdateLocationMismatch { old: (unwrap metadata).location, new: location })
        _ -> do
          lift $ onSuccess env (updateLocationMatches unit)
          pure metadata

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

locationIsUnique :: Location -> Map PackageName Metadata -> Maybe PackageName
locationIsUnique location = map _.key <<< Map.findMin <<< Map.filter (eq location <<< _.location <<< unwrap)
