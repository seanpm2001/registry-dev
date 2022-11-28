module Registry.Effect.Cache where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Const (Const(..))
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), hush)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe')
import Data.String as String
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import JSURI as JSURI
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Partial.Unsafe (unsafeCrashWith)
import Registry.App.Prelude (nowUTC)
import Registry.Effect.Log (LOG)
import Registry.Effect.Log as Log
import Registry.Internal.Codec as Internal.Codec
import Run (AFF, Run)
import Run as Run
import Run.Reader as Run.Reader
import Run.State (STATE)
import Run.State as Run.State
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
-- | data Key c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | getItem :: forall a r. CacheKey Key a -> Run (CACHE Key + r) (Maybe (CacheEntry a))
-- | getItem key = Run.lift _cache (get key)
-- | ```
get :: forall k a. CacheKey k a -> Cache k (Maybe (CacheEntry a))
get key = Get (key (Reply identity))

-- | Put a value in the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'put' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data Key c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | putItem :: forall a r. CacheKey Key a -> Run (CACHE Key + r) (Maybe a)
-- | putItem key = Run.lift _cache (get key)
-- | ```
put :: forall k a. CacheKey k a -> a -> Cache k Unit
put key value = Put (key (Const value)) unit

-- | Delete a key from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'delete' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data Key c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | deleteItem :: forall a r. CacheKey Key a -> Run (CACHE Key + r) (Maybe a)
-- | deleteItem key = Run.lift _cache (get key)
-- | ```
delete :: forall k a. CacheKey k a -> Cache k Unit
delete key = Delete (key (Ignore unit))

type CACHE key r = (cache :: Cache key | r)

_cache :: Proxy "cache"
_cache = Proxy

-- | Given some type `a` to be stored in the cache, provides a unique identifier
-- | for a value of that type and a codec for encoding and decoding it as JSON.
type JsonKey a =
  { id :: String
  , codec :: JsonCodec a
  }

-- | Convert a cache identifier into a safe file path.
safePath :: String -> FilePath
safePath id =
  maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id

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
handleCacheFileSystem :: forall key a r. JsonKeyHandler key -> Cache key a -> Run (LOG + AFF + r) a
handleCacheFileSystem handler = Run.Reader.runReader (unsafePerformEffect (Ref.new Map.empty)) <<< case _ of
  -- TODO: Expire entries after they've not been fetched for 30 seconds?
  Get key -> handler key # Exists.runExists \(JsonKey { id, codec } (Reply reply)) -> do
    readMemory >>= Map.lookup id >>> case _ of
      Nothing -> do
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 (safePath id))) >>= case _ of
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
        Run.liftAff (Aff.attempt (FS.Aff.writeTextFile UTF8 (safePath id) (Argonaut.Core.stringify encoded))) >>= case _ of
          Left error -> Log.debug $ "Unable to write cache entry to file system: " <> Aff.message error
          Right _ -> Log.debug "Wrote cache entry!"
        pure next

  Delete key -> handler key # Exists.runExists \(JsonKey { id } (Ignore next)) -> do
    deleteMemoryAndDisk id
    pure next

  where
  readMemory = do
    memoryRef <- Run.Reader.ask
    Run.liftAff $ liftEffect $ Ref.read memoryRef

  modifyMemory k = do
    memoryRef <- Run.Reader.ask
    Run.liftAff $ liftEffect $ Ref.modify_ k memoryRef

  deleteMemoryAndDisk id = do
    modifyMemory (Map.delete id)
    Run.liftAff (Aff.attempt (FS.Aff.rm (safePath id))) >>= case _ of
      Left fsError -> Log.debug $ "Could not delete file system entry: " <> Aff.message fsError
      Right _ -> pure unit
