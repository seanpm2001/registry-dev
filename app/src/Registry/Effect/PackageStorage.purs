module Registry.Effect.PackageStorage
  ( PackageStorage
  , PACKAGE_STORAGE
  , _packageStorage
  , upload
  , download
  , delete
  , handlePackageStorageS3
  ) where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.HTTP.Method (Method(..))
import Foreign.S3 as S3
import Node.Buffer as Buffer
import Registry.Constants as Constants
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))

data PackageStorage a
  = Upload PackageName Version Buffer a
  | Download PackageName Version (Buffer -> a)
  | Delete PackageName Version a

derive instance Functor PackageStorage

type PACKAGE_STORAGE r = (packageStorage :: PackageStorage | r)

_packageStorage :: Proxy "packageStorage"
_packageStorage = Proxy

upload :: forall r. PackageName -> Version -> Buffer -> Run (PACKAGE_STORAGE + r) Unit
upload name version buffer = Run.lift _packageStorage (Upload name version buffer unit)

download :: forall r. PackageName -> Version -> Run (PACKAGE_STORAGE + r) Buffer
download name version = Run.lift _packageStorage (Download name version identity)

delete :: forall r. PackageName -> Version -> Run (PACKAGE_STORAGE + r) Unit
delete name version = Run.lift _packageStorage (Delete name version unit)

connect :: forall r. Run (LOG + AFF + r) S3.Space
connect = do
  let bucket = "purescript-registry"
  Log.debug $ "Connecting to the bucket " <> bucket
  Run.liftAff (withBackoff' (S3.connect "ams3.digitaloceanspaces.com" bucket)) >>= case _ of
    Nothing -> Log.die "Timed out when attempting to connect to S3."
    Just connection -> pure connection

packageUrl :: PackageName -> Version -> Affjax.Node.URL
packageUrl name version = Array.fold
  [ Constants.packageStorageUrl
  , "/"
  , PackageName.print name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

-- | Handle package storage using a remote S3 bucket
handlePackageStorageS3
  :: forall r a
   . PackageStorage a
  -> Run (LOG + AFF + EFFECT + r) a
handlePackageStorageS3 = case _ of
  Upload name version buffer next -> do
    s3 <- connect
    pure next

  Download name version reply -> do
    response <- Run.liftAff $ Affjax.Node.request $ Affjax.Node.defaultRequest
      { method = Left GET
      , responseFormat = ResponseFormat.arrayBuffer
      , url = packageUrl name version
      }

    -- TODO: Rely on the metadata to check the size and hash?
    case response of
      Left err ->
        Log.die $ "Could not fetch package:\n " <> Affjax.Node.printError err
      Right { status, body } -> do
        buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
        pure (reply buffer)

  Delete name version next -> do
    pure next
