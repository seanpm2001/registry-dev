module Registry.Effect.Index where

import Registry.App.Prelude

import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))

data PackageIndex a
  = ReadMetadataIndex (Map PackageName Metadata -> a)
  | ReadMetadata PackageName (Maybe Metadata -> a)
  | WriteMetadata PackageName Metadata a
  | ReadManifestIndex (ManifestIndex -> a)
  | ReadManifest PackageName Version (Maybe Manifest -> a)
  | WriteManifest Manifest a
  | DeleteManifest PackageName Version a

derive instance Functor PackageIndex

type INDEX r = (index :: PackageIndex | r)

_index :: Proxy "index"
_index = Proxy

readMetadataIndex :: forall r. Run (INDEX + r) (Map PackageName Metadata)
readMetadataIndex = Run.lift _index (ReadMetadataIndex identity)

readMetadata :: forall r. PackageName -> Run (INDEX + r) (Maybe Metadata)
readMetadata name = Run.lift _index (ReadMetadata name identity)

writeMetadata :: forall r. PackageName -> Metadata -> Run (INDEX + r) Unit
writeMetadata name metadata = Run.lift _index (WriteMetadata name metadata unit)

readManifestIndex :: forall r. Run (INDEX + r) ManifestIndex
readManifestIndex = Run.lift _index (ReadManifestIndex identity)

readManifest :: forall r. PackageName -> Version -> Run (INDEX + r) (Maybe Manifest)
readManifest name version = Run.lift _index (ReadManifest name version identity)

writeManifest :: forall r. Manifest -> Run (INDEX + r) Unit
writeManifest manifest = Run.lift _index (WriteManifest manifest unit)

deleteManifest :: forall r. PackageName -> Version -> Run (INDEX + r) Unit
deleteManifest name version = Run.lift _index (DeleteManifest name version unit)
