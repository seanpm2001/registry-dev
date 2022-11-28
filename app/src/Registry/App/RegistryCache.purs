-- TODO: Rename to 'Cache'.
module Registry.App.RegistryCache where

import Registry.App.Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Exists as Exists
import Foreign.GitHub (GitHubError)
import Foreign.GitHub as GitHub
import Registry.App.Types as Types
import Registry.Effect.Cache (class Functor2, CACHE, CacheEntry, CacheKey, JsonEncodedBox(..), JsonKeyHandler)
import Registry.Effect.Cache as Cache
import Registry.Manifest as Manifest
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Run (Run)
import Run as Run

-- | A typed key for the standard Registry cache. Caches using this key should
-- | use the 'get', 'put', and 'delete' functions from this module.
data RegistryCache (c :: Type -> Type -> Type) a
  = ManifestFile PackageName Version (c Manifest.Manifest a)
  | GitHubRequest GitHub.Route (c (Either GitHubError Json) a)
  | LegacyPackageSet String (c (Either GitHubError Types.LegacyPackageSet) a)
  | LegacyPackageSetUnion Sha256 (c Types.LegacyPackageSetUnion a)

-- Ideally, with quantified constraints, this could be written as:
--   (forall x. Functor (c x)) => Functor (RegistryCache c)
-- but since PureScript doesn't have them, we lean on a 'Functor2' class and
-- a manual instance.
instance Functor2 c => Functor (RegistryCache c) where
  map k = case _ of
    ManifestFile name version a -> ManifestFile name version (Cache.map2 k a)
    GitHubRequest route a -> GitHubRequest route (Cache.map2 k a)
    LegacyPackageSet ref a -> LegacyPackageSet ref (Cache.map2 k a)
    LegacyPackageSetUnion tagsHash a -> LegacyPackageSetUnion tagsHash (Cache.map2 k a)

-- | A handler for the RegistryCache key for JSON caches. Can be used to
-- | implement interpreters for the CACHE effect:
-- |
-- | ```purs
-- | -- some interpreter for the Cache effect given any key type
-- | handleCache :: forall key a. JsonKeyHandler key -> Cache key a -> Run _ a
-- |
-- | -- a handler for the Cache effect for the RegistryCache key
-- | handleRegistryCache :: forall a. Cache RegistryCache a -> Run _ a
-- | handleRegistryCache = handleCache keyHandler
-- | ```
keyHandler :: JsonKeyHandler RegistryCache
keyHandler = case _ of
  ManifestFile name version next -> Exists.mkExists $ flip JsonKey next
    { id: "ManifestFile__" <> PackageName.print name <> "__" <> Version.print version
    , codec: Manifest.codec
    }
  GitHubRequest route next -> Exists.mkExists $ flip JsonKey next
    { id: "GitHubRequest__" <> GitHub.printRoute route
    , codec: CA.Common.either GitHub.githubErrorCodec CA.json
    }
  LegacyPackageSet ref next -> Exists.mkExists $ flip JsonKey next
    { id: "LegacyPackageSet__" <> ref
    , codec: CA.Common.either GitHub.githubErrorCodec Types.legacyPackageSetCodec
    }
  LegacyPackageSetUnion tagsHash next -> Exists.mkExists $ flip JsonKey next
    { id: "LegacyPackageSetUnion__" <> Sha256.print tagsHash
    , codec: Types.legacyPackageSetUnionCodec
    }

-- | Get an item from the registry cache.
-- |
-- | ```purs
-- | getPrelude :: forall r. Run (CACHE RegistryCache + r) (Maybe Manifest)
-- | getPrelude = get (ManifestFile (PackageName "prelude") (Version "1.0.0")
-- | ```
get :: forall a r. CacheKey RegistryCache a -> Run (CACHE RegistryCache + r) (Maybe (CacheEntry a))
get key = Run.lift Cache._cache (Cache.get key)

-- | Put an item from the registry cache.
-- |
-- | ```purs
-- | putRequest :: forall r. Run (CACHE RegistryCache + r) Unit
-- | putRequest = put (GitHubRequest (Route "GET /") "json"
-- | ```
put :: forall a r. CacheKey RegistryCache a -> a -> Run (CACHE RegistryCache + r) Unit
put key val = Run.lift Cache._cache (Cache.put key val)

-- | Delete an item from the registry cache.
-- |
-- | ```purs
-- | deleteRequest :: forall r. Run (CACHE RegistryCache + r) Unit
-- | deleteRequest = delete (GitHubRequest (Route "GET /")
-- | ```
delete :: forall a r. CacheKey RegistryCache a -> Run (CACHE RegistryCache + r) Unit
delete key = Run.lift Cache._cache (Cache.delete key)
