module Registry.Effect.Index where

import Registry.App.Prelude

data Index a
  = ReadMetadataIndex (Map PackageName Metadata -> a)
  | ReadMetadata PackageName (Maybe Metadata -> a)
  | WriteMetadata PackageName Metadata a
  | ReadManifestIndex (ManifestIndex -> a)
  | ReadManifest PackageName Version (Maybe Manifest -> a)
  | WriteManifest Manifest a
  | DeleteManifest PackageName Version a
