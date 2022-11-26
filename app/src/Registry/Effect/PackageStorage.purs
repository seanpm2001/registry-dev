module Registry.Effect.PackageStorage where

import Registry.App.Prelude

data PackageStorage a
  = Upload PackageName Version Buffer a
  | Download PackageName Version (Buffer -> a)
  | Delete PackageName Version a
