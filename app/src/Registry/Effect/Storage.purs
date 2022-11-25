module Registry.Effect.PackageStorage where

import Registry.App.Prelude

data StorageF a
  = UploadPackage PackageName Version Buffer a
  | DownloadPackage PackageName Version (Buffer -> a)
  | DeletePackage PackageName Version a
