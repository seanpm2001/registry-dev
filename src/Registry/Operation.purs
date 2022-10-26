module Registry.Operation where

import Registry.Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Validation.Semigroup as Validation
import Foreign.FastGlob as FastGlob
import Foreign.Node.FS as FS.Extra
import Foreign.SPDX (License)
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Node.Path as Path
import Registry.Index (RegistryIndex)
import Registry.Json ((.:))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location, Manifest(..), Metadata, Owner)
import Registry.Solver as Solver
import Registry.Types (Log)
import Registry.Version (Version)
import Registry.Version as Version

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
  metadataUpdates <- ExceptT $ pure $ verifyPackageInfo env payload manifest metadata
  -- verifyPackageSource

  -- They'll pass in a filepath to a tarball, we check the size
  -- don't check ignored files aren't in the tarball - we don't touch tarball
  -- API only means don't put it here

  pure unit

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
  -> Either (Array OperationError) (Metadata -> Metadata)
verifyPackageInfo env publishData manifest metadata =
  Validation.toEither ado
    update1 <- packageNamesMatch publishData manifest
    update2 <- notMetadataPackage manifest
    update3 <- versionDoesNotExist manifest metadata
    update4 <- locationsMatch manifest metadata
    update5 <- ownersMatch manifest metadata
    update6 <- packageDependenciesSolve manifest env.registryIndex
    -- update7 <- licenseMatch env manifest
    in update1 >>> update2 >>> update3 >>> update4 >>> update5 >>> update6

packageNamesMatch :: PublishData -> Manifest -> Validation.V (Array OperationError) (Metadata -> Metadata)
packageNamesMatch { name: apiName } (Manifest { name: manifestName }) =
  if apiName /= manifestName then
    Validation.invalid (Array.singleton PackageNameMismatch)
  else
    pure (\i -> i)

notMetadataPackage :: Manifest -> Validation.V (Array OperationError) (Metadata -> Metadata)
notMetadataPackage (Manifest { name }) =
  if PackageName.print name == "metadata" then
    Validation.invalid (Array.singleton MetadataPackageUpload)
  else
    pure identity

versionDoesNotExist :: Manifest -> Metadata -> Validation.V (Array OperationError) (Metadata -> Metadata)
versionDoesNotExist (Manifest { version }) metadata = ado
  _ <- case Map.lookup version metadata.published of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyPublished)
    Nothing -> pure (\i -> i)
  _ <- case Map.lookup version metadata.unpublished of
    Just _ -> Validation.invalid (Array.singleton VersionAlreadyUnpublished)
    Nothing -> pure (\i -> i)
  in identity

locationsMatch :: Manifest -> Metadata -> Validation.V (Array OperationError) (Metadata -> Metadata)
locationsMatch (Manifest { location }) { location: metadataLocation } =
  if location /= metadataLocation then
    Validation.invalid (Array.singleton LocationMismatch)
  else
    pure identity

ownersMatch :: Manifest -> Metadata -> Validation.V (Array OperationError) (Metadata -> Metadata)
ownersMatch (Manifest { owners: manifestOwners }) { owners: metadataOwners } =
  if manifestOwners /= metadataOwners then
    pure (_ { owners = manifestOwners })
  else
    pure (\i -> i)

-- If they gave us resolutions, then we skip this step.
packageDependenciesSolve :: Manifest -> RegistryIndex -> Validation.V (Array OperationError) (Metadata -> Metadata)
packageDependenciesSolve (Manifest { dependencies }) registryIndex =
  case Solver.solve (map (map (unwrap >>> _.dependencies)) registryIndex) dependencies of
    Left _ -> Validation.invalid (Array.singleton PackageDidNotSolve)
    Right _ -> pure identity

-- Should already have the package source
licenseMatch :: forall m. Monad m => MonadAff m => OperationEnv m -> Manifest -> m (Validation.V (Array OperationError) (Metadata -> Metadata))
licenseMatch env (Manifest { license }) = do
  packageSource <- env.fetchPackageSource
  licenseResult <- env.detectLicense packageSource license
  case licenseResult of
    Left _ -> pure $ Validation.invalid (Array.singleton LicenseMismatch)
    Right sourceLicense ->
      if license /= sourceLicense then
        pure $ Validation.invalid (Array.singleton LicenseMismatch)
      else
        pure $ pure identity

verifyPackageSource
  :: forall m
   . Monad m 
  => MonadAff m 
  => OperationEnv m 
  -> Manifest
  -> FilePath 
  -> m (Either OperationError Unit)
verifyPackageSource env (Manifest manifest) packageSource = runExceptT do
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  let newDirname = PackageName.print manifest.name <> "-" <> Version.printVersion manifest.version
  let packageSourceDir = Path.concat [ tmpDir, newDirname ]
  ExceptT $ copyPackageSourceFiles manifest.files { source: packageSource, destination: packageSourceDir }
  lift $ removeIgnoredTarballFiles tmpDir
  let tarballPath = packageSource <> ".tar.gz"
  lift $ env.createTar { cwd: tmpDir, folderName: newDirname }
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.stat tarballPath
  -- when (bytes > warnPackageBytes) do
  when (bytes > maxPackageBytes) do
    throwError PackageTarBallSizeExceedsMaximum

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

-- | Copy files from the package source directory to the destination directory
-- | for the tarball. This will copy all always-included files as well as files
-- | provided by the user via the `files` key.

-- TODO: Don't actually copy, just run validation on source
-- TODO: Have the user pass us a glob matcher - we'll give them the globs, then we will sanitize filepaths & run check
copyPackageSourceFiles :: forall m. MonadAff m => Maybe (Array String) -> { source :: FilePath, destination :: FilePath } -> m (Either OperationError Unit)
copyPackageSourceFiles files { source, destination } = runExceptT do
  userFiles <- case files of
    Nothing -> pure []
    Just globs -> do
      { succeeded, failed } <- liftAff $ FastGlob.match source globs

      unless (Array.null failed) do
        throwError PackageSourceFilesOutsideDirectory
          -- String.joinWith " "
          -- [ "Some paths matched by globs in the 'files' key are outside your package directory."
          -- , "Please ensure globs only match within your package directory, including symlinks."
          -- ]

      pure succeeded

  includedFiles <- liftAff $ FastGlob.match source includedGlobs
  includedInsensitiveFiles <- liftAff $ FastGlob.match' source includedInsensitiveGlobs { caseSensitive: false }

  let
    copyFiles = userFiles <> includedFiles.succeeded <> includedInsensitiveFiles.succeeded
    makePaths path = { from: Path.concat [ source, path ], to: Path.concat [ destination, path ], preserveTimestamps: true }

  liftAff $ traverse_ (makePaths >>> FS.Extra.copy) copyFiles

-- | We always include some files and directories when packaging a tarball, in
-- | addition to files users opt-in to with the 'files' key.
includedGlobs :: Array String
includedGlobs =
  [ "src/"
  , "purs.json"
  , "spago.dhall"
  , "packages.dhall"
  , "bower.json"
  , "package.json"
  ]

-- | These files are always included and should be globbed in case-insensitive
-- | mode.
includedInsensitiveGlobs :: Array String
includedInsensitiveGlobs =
  [ "README*"
  , "LICENSE*"
  , "LICENCE*"
  ]

-- | We always ignore some files and directories when packaging a tarball, such
-- | as common version control directories, even if a user has explicitly opted
-- | in to those files with the 'files' key.
-- |
-- | See also:
-- | https://docs.npmjs.com/cli/v8/configuring-npm/package-json#files
removeIgnoredTarballFiles :: forall m. Monad m => MonadAff m => FilePath -> m Unit
removeIgnoredTarballFiles path = do
  globMatches <- liftAff $ FastGlob.match' path ignoredGlobs { caseSensitive: false }
  for_ (ignoredDirectories <> ignoredFiles <> globMatches.succeeded) \match ->
    liftAff $ FS.Extra.remove (Path.concat [ path, match ])

ignoredDirectories :: Array FilePath
ignoredDirectories =
  [ ".psci"
  , ".psci_modules"
  , ".spago"
  , "node_modules"
  , "bower_components"
  -- These files and directories are ignored by the NPM CLI and we are
  -- following their lead in ignoring them as well.
  , ".git"
  , "CVS"
  , ".svn"
  , ".hg"
  ]

ignoredFiles :: Array FilePath
ignoredFiles =
  [ "package-lock.json"
  , "yarn.lock"
  , "pnpm-lock.yaml"
  ]

ignoredGlobs :: Array String
ignoredGlobs =
  [ "**/*.*.swp"
  , "**/._*"
  , "**/.DS_Store"
  ]

