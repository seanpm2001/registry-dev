module Registry.Effect.Cache where

import Prelude

import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Const (Const(..))
import Data.Either (hush)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Maybe (Maybe)
import Node.Path (FilePath)
import Run (AFF, Run)
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

derive instance (Functor (key Reply), Functor (key Ignore)) => Functor (Cache key)

type CacheKey :: ((Type -> Type -> Type) -> Type -> Type) -> Type -> Type
type CacheKey key a = forall c b. c a b -> key c b

get :: forall k a. CacheKey k a -> Cache k (Maybe a)
get key = Get (key (Reply identity))

put :: forall k a. CacheKey k a -> a -> Cache k Unit
put key value = Put (key (Const value)) unit

delete :: forall k a. CacheKey k a -> Cache k Unit
delete key = Delete (key (Ignore unit))

type CACHE key r = (cache :: Cache key | r)

_cache = Proxy :: Proxy "cache"

type FileSystemKey a =
  { path :: FilePath
  , codec :: JsonCodec a
  }

type FileSystemKeyHandler key = forall b z. key z b -> FileSystem z b

data FileSystemBox :: (Type -> Type -> Type) -> Type -> Type -> Type
data FileSystemBox z b a = FileSystem (FileSystemKey a) (z a b)

type FileSystem z b = Exists (FileSystemBox z b)

runCacheFileSystem
  :: forall key a r
   . FileSystemKeyHandler key
  -> Cache key a
  -> Run (CACHE key + AFF + r) a
runCacheFileSystem handler = case _ of
  Get key -> handler key # Exists.runExists \(FileSystem { path, codec } (Reply reply)) -> do
    let decoded = hush <<< CA.decode codec =<< hush (Argonaut.Parser.jsonParser "")
    pure (reply decoded)

  Put key next -> handler key # Exists.runExists \(FileSystem { path, codec } (Const value)) -> do
    let encoded = Argonaut.Core.stringify $ CA.encode codec value
    pure next

  Delete key -> handler key # Exists.runExists \(FileSystem { path, codec } (Ignore next)) -> do
    pure next

----------
-- Example interpreter, using key
----------

data RegistryCache :: (Type -> Type -> Type) -> Type -> Type
data RegistryCache k a = ConfigKey Int (k Int a)

registryCacheKeyHandler :: FileSystemKeyHandler RegistryCache
registryCacheKeyHandler = case _ of
  ConfigKey id next -> Exists.mkExists $ FileSystem { path: show id, codec: CA.int } next

runCache :: forall a r. Cache RegistryCache a -> Run (CACHE RegistryCache + AFF + r) a
runCache = runCacheFileSystem registryCacheKeyHandler
