module Registry.Effect.PackageSets where

import Registry.App.Prelude

data Process = Atomic | Sequential

type SequentialUpgradeResult =
  { failed :: Map PackageName (Maybe Version)
  , succeeded :: Map PackageName (Maybe Version)
  , result :: PackageSet
  }

data PackageSets a
  = ReadAll (Map Version PackageSet -> a)
  | ReadLatest (Maybe PackageSet -> a)
  | UpgradeAtomic PackageSet Version (Map PackageName (Maybe Version)) (Either (Map PackageName (Maybe Version)) PackageSet -> a)
  | UpgradeSequential PackageSet Version (Map PackageName (Maybe Version)) (SequentialUpgradeResult -> a)
