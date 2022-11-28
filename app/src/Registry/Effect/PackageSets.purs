module Registry.Effect.PackageSets where

import Registry.App.Prelude

import Data.Map as Map
import Registry.Effect.GitHub (REGISTRY_REPO)
import Registry.Effect.Log (LOG)
import Registry.Effect.Storage (STORAGE)
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))

data Change = Update Version | Remove

derive instance Eq Change

type ChangeSet = Map PackageName Change

type SequentialUpgradeResult =
  { failed :: ChangeSet
  , succeeded :: ChangeSet
  , result :: PackageSet
  }

data PackageSets a
  = ReadAll (Map Version PackageSet -> a)
  | ReadLatest (Maybe PackageSet -> a)
  | UpgradeAtomic PackageSet Version ChangeSet (Either ChangeSet PackageSet -> a)
  | UpgradeSequential PackageSet Version ChangeSet (SequentialUpgradeResult -> a)

derive instance Functor PackageSets

type PACKAGE_SETS r = (packageSets :: PackageSets | r)

_packageSets :: Proxy "packageSets"
_packageSets = Proxy

-- | Read all package sets published by the registry.
readAll :: forall r. Run (PACKAGE_SETS + r) (Map Version PackageSet)
readAll = Run.lift _packageSets (ReadAll identity)

-- | Read the latest package set published by the registry.
readLatest :: forall r. Run (PACKAGE_SETS + r) (Maybe PackageSet)
readLatest = Run.lift _packageSets (ReadLatest identity)

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. If any change fails, then the upgrade is aborted and the
-- | unsuccessful changes are returned.
upgradeAtomic :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + r) (Either ChangeSet PackageSet)
upgradeAtomic oldSet compiler changes = Run.lift _packageSets (UpgradeAtomic oldSet compiler changes identity)

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. Any successful change is applied, and any unsuccessful changes
-- | are returned along with the new package set.
upgradeSequential :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + r) SequentialUpgradeResult
upgradeSequential oldSet compiler changes = Run.lift _packageSets (UpgradeSequential oldSet compiler changes identity)

-- | A handler for the PACKAGE_SETS effect which attempts to compile the package
-- | sets and commit the results.
handlePackageSets :: forall r a. PackageSets a -> Run (REGISTRY_REPO + STORAGE + LOG + AFF + r) a
handlePackageSets = case _ of
  ReadAll reply ->
    pure $ reply Map.empty

  ReadLatest reply ->
    pure $ reply Nothing

  UpgradeAtomic _oldSet _compiler changes reply ->
    pure $ reply (Left changes)

  UpgradeSequential oldSet _compiler changes reply ->
    pure $ reply { failed: changes, succeeded: changes, result: oldSet }
