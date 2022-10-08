module Z3 where

import Registry.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable as Traversable
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object as Object
import Node.Process (exit)
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..))
import Registry.Version (Range, Version)
import Registry.Version as Version
import Unsafe.Coerce (unsafeCoerce)

foreign import data Solver :: Type
foreign import data Variables :: Type
foreign import newSolverImpl :: EffectFn1 String (Promise Solver)
foreign import addVariablesImpl :: EffectFn2 Solver (Array String) Variables
foreign import solveImpl :: EffectFn3 Solver Variables JSClause (Promise (Object Int))

newSolver :: String -> Aff Solver
newSolver = Promise.toAffE <<< runEffectFn1 newSolverImpl

addVariables :: Solver -> Array String -> Aff Variables
addVariables s = liftEffect <<< runEffectFn2 addVariablesImpl s

solve :: Solver -> Variables -> Z3Clause Int -> Aff (Object Int)
solve s vs clause = do
  let
    toJsClause :: Z3Clause Int -> JSClause
    toJsClause = case _ of
      ZGE pkg v -> { op: "ge", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZLT pkg v -> { op: "lt", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZEQ pkg v -> { op: "eq", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZOR cs -> { op: "or", l: unsafeCoerce (map toJsClause cs), r: unsafeCoerce unit }
      ZAND cs -> { op: "and", l: unsafeCoerce (map toJsClause cs), r: unsafeCoerce unit }
      ZIMP lhs rhs -> { op: "implies", l: unsafeCoerce (toJsClause lhs), r: unsafeCoerce (toJsClause rhs) }
      ZIFF lhs rhs -> { op: "iff", l: unsafeCoerce (toJsClause lhs), r: unsafeCoerce (toJsClause rhs) }
  Promise.toAffE $ runEffectFn3 solveImpl s vs (toJsClause clause)

type JSClause = { op :: String, l :: Foreign, r :: Foreign }

data Z3Clause a
  = ZOR (Array (Z3Clause a))
  | ZAND (Array (Z3Clause a))
  | ZIMP (Z3Clause a) (Z3Clause a)
  | ZIFF (Z3Clause a) (Z3Clause a)
  | ZEQ PackageName a
  | ZGE PackageName a
  | ZLT PackageName a

instance Show a => Show (Z3Clause a) where
  show = case _ of
    ZGE pkg v -> show pkg <> " >= " <> show v
    ZLT pkg v -> show pkg <> " < " <> show v
    ZEQ pkg v -> show pkg <> " == " <> show v
    ZOR cs -> "OR " <> show cs
    ZAND cs -> "AND " <> show cs
    ZIMP lhs rhs -> "IMPLIES: " <> show lhs <> " => " <> show rhs
    ZIFF lhs rhs -> "IFF: " <> show lhs <> " <=> " <> show rhs

derive instance Eq a => Eq (Z3Clause a)
derive instance Functor Z3Clause

main :: Effect Unit
main = launchAff_ do
  registryIndex <- liftAff $ Index.readRegistryIndex "/Users/fabrizio/Library/Caches/spago-nodejs/registry-index"
  log $ show $ Map.size registryIndex
  -- Solving "aff" "7.1.0"
  let lookupVersions pkg = just $ Map.lookup (right $ PackageName.parse pkg) registryIndex
  let pkg = "aff" -- "halogen"
  let vs = "7.1.0" -- "6.1.2"
  let
    Manifest manifest = just do
      versions <- Map.lookup (right $ PackageName.parse pkg) registryIndex
      manifest <- Map.lookup (right $ Version.parseVersion Version.Lenient vs) versions --  versions
      pure manifest
  -- log $ show manifest.dependencies
  let
    clausesForManifest manifest = map
      ( \(Tuple packageName range) ->
          let
            manifests = map unwrap $ versionsForRange (lookupVersions (PackageName.print packageName)) range
            rangeClause = ZAND [ ZGE packageName (Version.greaterThanOrEq range), ZLT packageName (Version.lessThan range) ]
            mkImplication innerManifest = ZIMP (ZEQ packageName innerManifest.version) (ZAND $ clausesForManifest innerManifest)
            implicationClause = ZAND $ map mkImplication manifests
          in
            ZAND [ rangeClause, implicationClause ]
      )
      (Map.toUnfoldable manifest.dependencies :: Array (Tuple PackageName Range))
  let (affVersionClauses :: Z3Clause Version) = ZAND $ clausesForManifest manifest
  -- log $ show affVersionClauses
  affIntClauses :: Z3Clause Int <- liftEffect $ convertToInts registryIndex affVersionClauses
  -- log $ show affIntClauses
  newAffVersionClauses :: Z3Clause Version <- liftEffect $ convertToVersion registryIndex affIntClauses
  -- when (newAffVersionClauses /= affVersionClauses) do
  --  unsafeCrashWith "clauses diverge"
  let names = getNames newAffVersionClauses
  log $ show names
  log "Getting new solver"
  solver <- newSolver "main"
  log "Adding variables"
  vars <- addVariables solver (map PackageName.print $ Set.toUnfoldable names)
  log "Solving"
  result :: Object Int <- solve solver vars affIntClauses
  resultsArray <- for (Object.toUnfoldable result :: Array (Tuple String Int)) \(Tuple pkgStr n) -> do
    let pkg' = right $ PackageName.parse pkgStr
    newVersion <- liftEffect $ toVersion registryIndex pkg' n
    pure (Tuple pkg' newVersion)
  log $ show resultsArray
  liftEffect $ exit 0

just :: forall a. Maybe a -> a
just = fromJust' (\_ -> unsafeCrashWith "this should be a just")

right :: forall a b. Either a b -> b
right = fromRight' (\_ -> unsafeCrashWith "this should be a right")

versionsForRange :: Map Version Manifest -> Range -> Array Manifest
versionsForRange versions range = List.toUnfoldable $ Map.values $ Map.filterWithKey (\v _m -> Version.rangeIncludes range v) versions

type LookupTable =
  { toInt :: Map Version Int
  , toVersion :: Map Int Version
  }

cacheRef :: Ref (Map PackageName LookupTable)
cacheRef = unsafePerformEffect (Ref.new Map.empty)

toInt :: RegistryIndex -> PackageName -> Version -> Effect Int
toInt registryIndex packageName version = do
  cache <- Ref.read cacheRef
  case Map.lookup packageName cache of
    Just lookupTable -> do
      case Map.lookup version lookupTable.toInt of
        Just i -> pure i
        Nothing -> do
          -- If we get a nothing here we are probably looking up a range that doesn't exist,
          -- i.e. <7.0.0, where 7.0.0 was never a version.
          -- So we check if it's at either threshold and add it to both lookup fns if yes
          let
            max = just $ Traversable.maximum $ Map.keys lookupTable.toInt
          -- min = just $ Traversable.minimum $ lookupTable.toInt
          if version > max then do
            let n = Map.size lookupTable.toInt
            let
              newLookupTable =
                { toInt: Map.insert version n lookupTable.toInt
                , toVersion: Map.insert n version lookupTable.toVersion
                }
            Ref.write (Map.insert packageName newLookupTable cache) cacheRef
            pure n
          else unsafeCrashWith "disaster"
    Nothing -> do
      let
        (versions :: Array Version) = Array.fromFoldable $ Map.keys $ just $ Map.lookup packageName registryIndex
        (lookupTable :: LookupTable) =
          { toInt: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple v i) versions
          , toVersion: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple i v) versions
          }
      Ref.write (Map.insert packageName lookupTable cache) cacheRef
      pure $ unsafeLookup version lookupTable.toInt

toVersion :: RegistryIndex -> PackageName -> Int -> Effect Version
toVersion _registryIndex packageName version = do
  cache <- Ref.read cacheRef
  case Map.lookup packageName cache of
    Just lt -> pure $ unsafeLookup version lt.toVersion
    Nothing -> unsafeCrashWith "wrong"

convertToInts :: RegistryIndex -> Z3Clause Version -> Effect (Z3Clause Int)
convertToInts registryIndex = case _ of
  ZGE pkg v -> map (ZGE pkg) (toInt registryIndex pkg v)
  ZLT pkg v -> map (ZLT pkg) (toInt registryIndex pkg v)
  ZEQ pkg v -> map (ZEQ pkg) (toInt registryIndex pkg v)
  ZOR cs -> map ZOR $ traverse (convertToInts registryIndex) cs
  ZAND cs -> map ZAND $ traverse (convertToInts registryIndex) cs
  ZIMP lhs rhs -> do
    l <- convertToInts registryIndex lhs
    r <- convertToInts registryIndex rhs
    pure $ ZIMP l r
  ZIFF lhs rhs -> do
    l <- convertToInts registryIndex lhs
    r <- convertToInts registryIndex rhs
    pure $ ZIFF l r

convertToVersion :: RegistryIndex -> Z3Clause Int -> Effect (Z3Clause Version)
convertToVersion registryIndex = case _ of
  ZGE pkg v -> map (ZGE pkg) (toVersion registryIndex pkg v)
  ZLT pkg v -> map (ZLT pkg) (toVersion registryIndex pkg v)
  ZEQ pkg v -> map (ZEQ pkg) (toVersion registryIndex pkg v)
  ZOR cs -> map ZOR $ traverse (convertToVersion registryIndex) cs
  ZAND cs -> map ZAND $ traverse (convertToVersion registryIndex) cs
  ZIMP lhs rhs -> do
    l <- convertToVersion registryIndex lhs
    r <- convertToVersion registryIndex rhs
    pure $ ZIMP l r
  ZIFF lhs rhs -> do
    l <- convertToVersion registryIndex lhs
    r <- convertToVersion registryIndex rhs
    pure $ ZIFF l r

getNames :: forall a. Z3Clause a -> Set PackageName
getNames = case _ of
  ZGE pkg _ -> Set.singleton pkg
  ZLT pkg _ -> Set.singleton pkg
  ZEQ pkg _ -> Set.singleton pkg
  ZOR cs -> Set.unions (map getNames cs)
  ZAND cs -> Set.unions (map getNames cs)
  ZIMP l r -> Set.unions [ getNames l, getNames r ]
  ZIFF l r -> Set.unions [ getNames l, getNames r ]

unsafeLookup :: forall k v. Ord k => Show k => Show v => k -> Map k v -> v
unsafeLookup k m = case Map.lookup k m of
  Just a -> a
  Nothing -> unsafeCrashWith $ "Tried to lookup key " <> show k <> " from map: " <> show m
