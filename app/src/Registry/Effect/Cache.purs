module Registry.Effect.Cache where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Const (Const(..))
import Data.Either (hush)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe')
import Data.String as String
import JSURI as JSURI
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Partial.Unsafe (unsafeCrashWith)
import Registry.Effect.Log (LOG)
import Run (AFF, Run)
import Run as Run
import Run.Reader as Run.Reader
import Run.State (STATE)
import Run.State as Run.State
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

class Functor2 (c :: Type -> Type -> Type) where
  map2 :: forall a b z. (a -> b) -> c z a -> c z b

newtype Reply a b = Reply (Maybe a -> b)

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

-- | Get a value from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'get' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data Key c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | getItem :: forall a r. CacheKey Key a -> Run (CACHE Key + r) (Maybe a)
-- | getItem key = Run.lift _cache (get key)
-- | ```
get :: forall k a. CacheKey k a -> Cache k (Maybe a)
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

_cache = Proxy :: Proxy "cache"

type JsonKey a =
  { id :: String
  , codec :: JsonCodec a
  }

safePath :: String -> FilePath
safePath id =
  maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id

type JsonKeyHandler key = forall b z. key z b -> JsonEncoded z b

data JsonEncodedBox :: (Type -> Type -> Type) -> Type -> Type -> Type
data JsonEncodedBox z b a = JsonKey (JsonKey a) (z a b)

type JsonEncoded z b = Exists (JsonEncodedBox z b)

-- | Run a cache in memory only.
handleCachePure :: forall key a r. JsonKeyHandler key -> Cache key a -> Run (STATE (Map String Json) + r) a
handleCachePure handler = case _ of
  Get key -> handler key # Exists.runExists \(JsonKey { id, codec } (Reply reply)) -> do
    state <- Run.State.get
    pure $ reply $ hush <<< CA.decode codec =<< Map.lookup id state

  Put key next -> handler key # Exists.runExists \(JsonKey { id, codec } (Const value)) -> do
    Run.State.modify (Map.insert id (CA.encode codec value))
    pure next

  Delete key -> handler key # Exists.runExists \(JsonKey { id } (Ignore next)) -> do
    Run.State.modify (Map.delete id)
    pure next

-- | Run a cache backed by the file system. The cache will try to minimize
-- | writes and reads to the file system by storing data in memory when possible.
handleCacheFileSystem
  :: forall key a r
   . JsonKeyHandler key
  -> Cache key a
  -> Run (LOG + AFF + r) a
handleCacheFileSystem handler = Run.Reader.runReader Map.empty <<< case _ of
  -- TODO: Expire entries after they've not been fetched for 30 seconds?
  Get key -> handler key # Exists.runExists \(JsonKey { id, codec } (Reply reply)) -> do
    let decode = hush <<< CA.decode codec
    memory <- Run.Reader.ask
    case Map.lookup id memory of
      Nothing -> do
        contents <- Run.liftAff $ FS.Aff.readTextFile UTF8 (safePath id)
        pure $ reply $ decode =<< hush (Argonaut.Parser.jsonParser contents)
      Just json ->
        pure $ reply $ decode json

  -- TODO: Record the current time? (Requires adding 'CacheEntry' to 'Reply')
  Put key next -> handler key # Exists.runExists \(JsonKey { id, codec } (Const value)) -> do
    let encoded = Argonaut.Core.stringify (CA.encode codec value)
    Run.liftAff $ FS.Aff.writeTextFile UTF8 (safePath id) encoded
    pure next

  Delete key -> handler key # Exists.runExists \(JsonKey { id } (Ignore next)) -> do
    Run.liftAff $ FS.Aff.rm (safePath id)
    pure next
