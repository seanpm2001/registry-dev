module Registry.Effect.Cache where

import Registry.App.Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Const (Const(..))
import Data.DateTime (DateTime)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map as Map
import Data.Maybe (maybe')
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Symbol as Symbol
import Effect.Aff as Aff
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubError, Route)
import Foreign.GitHub as GitHub
import JSURI as JSURI
import Node.FS.Aff as FS.Aff
import Registry.App.Types as Types
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.Internal.Codec as Internal.Codec
import Registry.Manifest as Manifest
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Reader as Run.Reader
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

class Functor2 (c :: Type -> Type -> Type) where
  map2 :: forall a b z. (a -> b) -> c z a -> c z b

newtype Reply a b = Reply (Maybe (CacheEntry a) -> b)

instance Functor2 Reply where
  map2 k (Reply f) = Reply (map k f)

newtype Ignore :: forall k. k -> Type -> Type
newtype Ignore a b = Ignore b

instance Functor2 Ignore where
  map2 k (Ignore b) = Ignore (k b)

-- | An effect for caching values with an extensible key to support multiple
-- | independent caches.
data Cache key a
  = Get (key Reply a)
  | Put (forall void. key Const void) a
  | Delete (key Ignore a)

derive instance (Functor (k Reply), Functor (k Ignore)) => Functor (Cache k)

type CacheKey :: ((Type -> Type -> Type) -> Type -> Type) -> Type -> Type
type CacheKey k a = forall c b. c a b -> k c b

type CacheEntry a =
  { modified :: DateTime
  , value :: a
  }

cacheEntryCodec :: forall a. JsonCodec a -> JsonCodec (CacheEntry a)
cacheEntryCodec codec = CA.Record.object "CacheEntry"
  { modified: Internal.Codec.iso8601DateTime
  , value: codec
  }

-- | Get a value from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'get' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | get :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe (CacheEntry a))
-- | get key = Run.lift _myCache (getCache key)
-- | ```
getCache :: forall k a. CacheKey k a -> Cache k (Maybe (CacheEntry a))
getCache key = Get (key (Reply identity))

-- | Put a value in the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'put' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | put :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | put key = Run.lift _myCache (putCache key)
-- | ```
putCache :: forall k a. CacheKey k a -> a -> Cache k Unit
putCache key value = Put (key (Const value)) unit

-- | Delete a key from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'delete' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | delete :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | delete key = Run.lift _myCache (deleteCache key)
-- | ```
deleteCache :: forall k a. CacheKey k a -> Cache k Unit
deleteCache key = Delete (key (Ignore unit))

-- | Given some type `a` to be stored in the cache, provides a unique identifier
-- | for a value of that type and a codec for encoding and decoding it as JSON.
type JsonKey a =
  { id :: String
  , codec :: JsonCodec a
  }

-- | Convert a cache identifier into a safe file path.
safePath :: String -> String -> FilePath
safePath prefix id =
  maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") (prefix <> "." <> id)

-- | A mapping of key types to a unique cache identifier and codec for encoding
-- | and decoding the value as JSON. This uses an existential encoding, so you
-- | must use `Exists.mkExists` to hide the value's type.
-- |
-- | ```purs
-- | import Data.Exists as Exists
-- | import Registry.Effect.Cache as Cache
-- | import Registry.Manifest as Manifest
-- |
-- | data Key c a = ManifestFile PackageName Version (c Manifest.Manifest a)
-- |
-- | keyHandler :: Cache.JsonKeyHandler Key
-- | keyHandler = case _ of
-- |   ManifestFile name version k -> do
-- |     let id = "ManifestFile" <> print name <> print version
-- |      Exists.mkExists $ Cache.JsonKey { id, codec: Manifest.codec } next
-- | ```
type JsonKeyHandler key = forall b z. key z b -> JsonEncoded z b

-- | A box that can be used with `Exists` to hide the value associated with a
-- | particular key constructor.
data JsonEncodedBox :: (Type -> Type -> Type) -> Type -> Type -> Type
data JsonEncodedBox z b a = JsonKey (JsonKey a) (z a b)

type JsonEncoded z b = Exists (JsonEncodedBox z b)

-- | Run a cache backed by the file system. The cache will try to minimize
-- | writes and reads to the file system by storing data in memory when possible.
-- | Takes the label of the cache effect in question to namespace caches in the
-- | file system.
handleCacheFileSystem :: forall sym key a r. IsSymbol sym => Proxy sym -> JsonKeyHandler key -> Cache key a -> Run (LOG + AFF + r) a
handleCacheFileSystem label handler = runWithMap <<< case _ of
  -- TODO: Expire entries after they've not been fetched for 30 seconds?
  Get key -> handler key # Exists.runExists \(JsonKey { id, codec } (Reply reply)) -> do
    readMemory >>= Map.lookup id >>> case _ of
      Nothing -> do
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 (safePath prefix id))) >>= case _ of
          Left error -> do
            Log.debug $ "Did not find " <> id <> " in memory or file system cache: " <> Aff.message error
            pure $ reply Nothing
          Right value -> do
            Log.debug $ "Found " <> id <> " in file system cache, writing to memory..."
            case Argonaut.Parser.jsonParser value of
              Left error -> do
                Log.debug $ "Invalid JSON for " <> id <> ":\n" <> value <> ").\nParsing failed with error: " <> error <> "\nRemoving from cache!"
                deleteMemoryAndDisk id
                pure $ reply Nothing
              Right json -> case CA.decode (cacheEntryCodec codec) json of
                Left error -> do
                  Log.debug $ "Failed to decode JSON for " <> id <> ":\n" <> value <> ").\nParsing failed with error: " <> CA.printJsonDecodeError error <> "\nRemoving from cache!"
                  deleteMemoryAndDisk id
                  pure $ reply Nothing
                Right decoded ->
                  pure $ reply $ Just decoded

      Just json -> do
        Log.debug $ "Found " <> id <> " in memory cache!"
        case CA.decode (cacheEntryCodec codec) json of
          Left error -> do
            Log.debug $ "Failed to decode JSON for " <> id <> ":\n" <> Argonaut.Core.stringify json <> ").\nParsing failed with error: " <> CA.printJsonDecodeError error <> "\nRemoving from cache!"
            deleteMemoryAndDisk id
            pure $ reply Nothing
          Right decoded ->
            pure $ reply $ Just decoded

  Put key next -> handler key # Exists.runExists \(JsonKey { id, codec } (Const value)) -> do
    Log.debug $ "Putting " <> id <> " in cache..."
    now <- Run.liftAff $ liftEffect nowUTC
    let encoded = CA.encode (cacheEntryCodec codec) { modified: now, value }
    readMemory >>= Map.lookup id >>> case _ of
      Just previous | encoded == previous -> do
        Log.debug "Put value matches old value, skipping update."
        pure next
      _ -> do
        modifyMemory (Map.insert id encoded)
        Run.liftAff (Aff.attempt (FS.Aff.writeTextFile UTF8 (safePath prefix id) (Argonaut.Core.stringify encoded))) >>= case _ of
          Left error -> Log.debug $ "Unable to write cache entry to file system: " <> Aff.message error
          Right _ -> Log.debug "Wrote cache entry!"
        pure next

  Delete key -> handler key # Exists.runExists \(JsonKey { id } (Ignore next)) -> do
    deleteMemoryAndDisk id
    pure next

  where
  prefix = Symbol.reflectSymbol label

  runWithMap = Run.Reader.runReader (unsafePerformEffect (Ref.new Map.empty))

  readMemory = do
    memoryRef <- Run.Reader.ask
    Run.liftAff $ liftEffect $ Ref.read memoryRef

  modifyMemory k = do
    memoryRef <- Run.Reader.ask
    Run.liftAff $ liftEffect $ Ref.modify_ k memoryRef

  deleteMemoryAndDisk id = do
    modifyMemory (Map.delete id)
    Run.liftAff (Aff.attempt (FS.Aff.rm (safePath prefix id))) >>= case _ of
      Left fsError -> Log.debug $ "Could not delete file system entry: " <> Aff.message fsError
      Right _ -> pure unit

-- | A typed key for the standard Registry cache. Caches using this key should
-- | use the 'get', 'put', and 'delete' functions from this module.
data RegistryCache (c :: Type -> Type -> Type) a
  = ManifestFile PackageName Version (c Manifest a)
  | GitHubRequest Route (c (Either GitHubError Json) a)
  | LegacyPackageSet String (c (Either GitHubError Types.LegacyPackageSet) a)
  | LegacyPackageSetUnion Sha256 (c Types.LegacyPackageSetUnion a)

-- Ideally, with quantified constraints, this could be written as:
--   (forall x. Functor (c x)) => Functor (RegistryCache c)
-- but since PureScript doesn't have them, we lean on a 'Functor2' class and
-- a manual instance.
instance Functor2 c => Functor (RegistryCache c) where
  map k = case _ of
    ManifestFile name version a -> ManifestFile name version (map2 k a)
    GitHubRequest route a -> GitHubRequest route (map2 k a)
    LegacyPackageSet ref a -> LegacyPackageSet ref (map2 k a)
    LegacyPackageSetUnion tagsHash a -> LegacyPackageSetUnion tagsHash (map2 k a)

-- | A cache covering the common types used in the registry. Can be combined
-- | with other caches so long as they use different labels.
type CACHE r = (cache :: Cache RegistryCache | r)

_cache :: Proxy "cache"
_cache = Proxy

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
-- | getPrelude :: forall r. Run (CACHE + r) (Maybe Manifest)
-- | getPrelude = get (ManifestFile (PackageName "prelude") (Version "1.0.0")
-- | ```
get :: forall a r. CacheKey RegistryCache a -> Run (CACHE + r) (Maybe (CacheEntry a))
get key = Run.lift _cache (getCache key)

-- | Put an item from the registry cache.
-- |
-- | ```purs
-- | putRequest :: forall r. Run (CACHE + r) Unit
-- | putRequest = put (GitHubRequest (Route "GET /") "json"
-- | ```
put :: forall a r. CacheKey RegistryCache a -> a -> Run (CACHE + r) Unit
put key val = Run.lift _cache (putCache key val)

-- | Delete an item from the registry cache.
-- |
-- | ```purs
-- | deleteRequest :: forall r. Run (CACHE + r) Unit
-- | deleteRequest = delete (GitHubRequest (Route "GET /")
-- | ```
delete :: forall a r. CacheKey RegistryCache a -> Run (CACHE + r) Unit
delete key = Run.lift _cache (deleteCache key)
