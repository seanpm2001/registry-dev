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

newtype Var = Var String

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show (Var s) = show s

type VarDecl = { typ :: String, name :: String }

foreign import data Solver :: Type
foreign import data Variables :: Type
foreign import newSolverImpl :: EffectFn1 String (Promise Solver)
foreign import addVariablesImpl :: EffectFn2 Solver (Array VarDecl) Variables
foreign import solveImpl :: EffectFn3 Solver Variables JSClause (Promise (Object Int))

newSolver :: String -> Aff Solver
newSolver = Promise.toAffE <<< runEffectFn1 newSolverImpl

addVariables :: Solver -> Array VarDecl -> Aff Variables
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
      ZIFF (Var lhs) rhs -> { op: "iff", l: unsafeCoerce lhs, r: unsafeCoerce (toJsClause rhs) }
      ZVAR (Var var) -> { op: "var", l: unsafeCoerce var, r: unsafeCoerce unit }
      ZTRU -> { op: "tru", l: unsafeCoerce unit, r: unsafeCoerce unit }
  Promise.toAffE $ runEffectFn3 solveImpl s vs (toJsClause clause)

type JSClause = { op :: String, l :: Foreign, r :: Foreign }

data Z3Clause a
  = ZOR (Array (Z3Clause a))
  | ZAND (Array (Z3Clause a))
  | ZIMP (Z3Clause a) (Z3Clause a)
  | ZIFF Var (Z3Clause a)
  | ZEQ PackageName a
  | ZGE PackageName a
  | ZLT PackageName a
  | ZVAR Var
  | ZTRU

instance Show a => Show (Z3Clause a) where
  show = case _ of
    ZGE pkg v -> show pkg <> " >= " <> show v
    ZLT pkg v -> show pkg <> " < " <> show v
    ZEQ pkg v -> show pkg <> " == " <> show v
    ZOR cs -> "OR " <> show cs
    ZAND cs -> "AND " <> show cs
    ZIMP lhs rhs -> "IMPLIES: " <> show lhs <> " => " <> show rhs
    ZIFF lhs rhs -> "IFF: " <> show lhs <> " <=> " <> show rhs
    ZVAR (Var var) -> var
    ZTRU -> "TRUE"

derive instance Eq a => Eq (Z3Clause a)
derive instance Ord a => Ord (Z3Clause a)
derive instance Functor Z3Clause

main :: Effect Unit
main = launchAff_ do
  registryIndex <- liftAff $ Index.readRegistryIndex "/Users/fabrizio/Library/Caches/spago-nodejs/registry-index"
  log $ show $ Map.size registryIndex

  -- Pick the package to solve
  let lookupVersions pkg = just $ Map.lookup (right $ PackageName.parse pkg) registryIndex
  -- let pkg = "aff"
  let pkg = "halogen"
  -- let vs = "7.1.0"
  let vs = "7.0.0"
  let
    Manifest manifest = just do
      versions <- Map.lookup (right $ PackageName.parse pkg) registryIndex
      manifest <- Map.lookup (right $ Version.parseVersion Version.Lenient vs) versions --  versions
      pure manifest

  -- Construct the clause tree
  clausesCacheRef <- liftEffect $ Ref.new Map.empty
  freshNamesRef <- liftEffect $ Ref.new 0
  let
    memoizeClausesForVersion implicationKey innerManifest = do
      clausesCache <- liftEffect $ Ref.read clausesCacheRef
      case Map.lookup implicationKey clausesCache of
        -- if we have seen this clause before then we just return its name
        Just { var } -> pure $ ZVAR var
        -- if not, we make a new variable, recur, and store the result in the cache
        Nothing -> do
          if Map.isEmpty innerManifest.dependencies then
            pure ZTRU
          else do
            clauses <- clausesForManifest innerManifest
            newVar <- map (\n -> "var" <> show n) $ liftEffect $ Ref.modify (_ + 1) freshNamesRef
            let
              newClause = case clauses of
                [] -> ZTRU
                [ singleClause ] -> singleClause
                _ -> ZAND clauses
            liftEffect $ Ref.modify_ (Map.insert implicationKey { var: Var newVar, clause: newClause }) clausesCacheRef
            pure $ ZVAR (Var newVar)
    clausesForManifest manifest = for (Map.toUnfoldable manifest.dependencies :: Array (Tuple PackageName Range))
      -- For every dependency range, add a constraint for the range itself, and a bunch of implications for each version in the range (recursive step)
      \(Tuple packageName range) -> do
        let
          manifests = map unwrap $ versionsForRange (lookupVersions (PackageName.print packageName)) range
          rangeClause = ZAND [ ZGE packageName (Version.greaterThanOrEq range), ZLT packageName (Version.lessThan range) ]
          implicationKey version = ZEQ packageName version
          mkImplication innerManifest = do
            rhsClause <- memoizeClausesForVersion (implicationKey innerManifest.version) innerManifest
            -- don't emit implication if rhs is just TRUE
            case rhsClause of
              ZTRU -> pure $ Nothing
              _ -> pure $ Just $ ZIMP (implicationKey innerManifest.version) rhsClause
        implicationClause :: Z3Clause Version <- map (ZAND <<< Array.catMaybes) $ traverse mkImplication manifests
        pure case implicationClause of
          ZAND [] -> rangeClause
          ZAND [ singleClause ] -> ZAND [ rangeClause, singleClause ]
          _ -> ZAND [ rangeClause, implicationClause ]

  manifestClauses <- clausesForManifest manifest
  cacheClauses <- map (Map.values >>> List.toUnfoldable >>> map \{ var, clause } -> ZIFF var clause) $ liftEffect $ Ref.read clausesCacheRef
  let (affVersionClauses :: Z3Clause Version) = ZAND $ manifestClauses <> cacheClauses

  log $ show affVersionClauses
  log "------------------------------------------------------------------------"
  affIntClauses :: Z3Clause Int <- liftEffect $ convertToInts registryIndex affVersionClauses
  -- log $ show affIntClauses
  newAffVersionClauses :: Z3Clause Version <- liftEffect $ convertToVersion registryIndex affIntClauses
  -- when (newAffVersionClauses /= affVersionClauses) do
  --  unsafeCrashWith "clauses diverge"
  let names = getNames newAffVersionClauses
  -- log $ show names
  log "Getting new solver"
  solver <- newSolver "main"
  log "Adding variables"
  vars <- addVariables solver (Set.toUnfoldable names)
  log "Solving"
  log "------------------------------------------------------------------------"
  -- TODO: Tell Z3 to optimise the sum of the package versions: https://www.philipzucker.com/z3-rise4fun/optimization.html
  result :: Object Int <- solve solver vars affIntClauses
  resultsArray <- for (Object.toUnfoldable result :: Array (Tuple String Int)) \(Tuple pkgStr n) -> do
    let pkg' = unsafePackageName pkgStr
    newVersion <- liftEffect $ toVersion registryIndex pkg' n
    pure (Tuple pkg' newVersion)
  log "Found a build plan:"
  void $ for resultsArray \(Tuple p v) -> log $ "  - " <> show p <> ": " <> show v
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
            min = just $ Traversable.minimum $ Map.keys lookupTable.toInt
          if version > max then do
            let n = Map.size lookupTable.toInt
            let
              newLookupTable =
                { toInt: Map.insert version n lookupTable.toInt
                , toVersion: Map.insert n version lookupTable.toVersion
                }
            Ref.write (Map.insert packageName newLookupTable cache) cacheRef
            pure n
          else if version < min then do
            let
              n = (-1)
              newLookupTable =
                { toInt: Map.insert version n lookupTable.toInt
                , toVersion: Map.insert n version lookupTable.toVersion
                }
            Ref.write (Map.insert packageName newLookupTable cache) cacheRef
            pure n
          else unsafeCrashWith $ "Tried to lookup key " <> show version <> " from map: " <> show lookupTable.toInt
    Nothing -> do
      let
        (versions :: Array Version) = Array.fromFoldable $ Map.keys $ just $ Map.lookup packageName registryIndex
        (lookupTable :: LookupTable) =
          { toInt: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple v i) versions
          , toVersion: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple i v) versions
          }
      Ref.write (Map.insert packageName lookupTable cache) cacheRef
      toInt registryIndex packageName version

toVersion :: RegistryIndex -> PackageName -> Int -> Effect Version
toVersion _registryIndex packageName version = do
  cache <- Ref.read cacheRef
  case Map.lookup packageName cache of
    Just lt -> pure $ unsafeLookup ("toVersionNotCached " <> PackageName.print packageName <> "\n" <> show lt) version lt.toVersion
    Nothing -> unsafeCrashWith "wrong"

convertToInts :: RegistryIndex -> Z3Clause Version -> Effect (Z3Clause Int)
convertToInts registryIndex = case _ of
  ZGE pkg v -> map (ZGE pkg) (toInt registryIndex pkg v)
  ZLT pkg v -> map (ZLT pkg) (toInt registryIndex pkg v)
  ZEQ pkg v -> map (ZEQ pkg) (toInt registryIndex pkg v)
  ZOR cs -> map ZOR $ traverse (convertToInts registryIndex) cs
  ZAND cs -> map ZAND $ traverse (convertToInts registryIndex) cs
  ZVAR v -> pure (ZVAR v)
  ZTRU -> pure ZTRU
  ZIMP lhs rhs -> do
    l <- convertToInts registryIndex lhs
    r <- convertToInts registryIndex rhs
    pure $ ZIMP l r
  ZIFF l rhs -> do
    r <- convertToInts registryIndex rhs
    pure $ ZIFF l r

convertToVersion :: RegistryIndex -> Z3Clause Int -> Effect (Z3Clause Version)
convertToVersion registryIndex = case _ of
  ZGE pkg v -> map (ZGE pkg) (toVersion registryIndex pkg v)
  ZLT pkg v -> map (ZLT pkg) (toVersion registryIndex pkg v)
  ZEQ pkg v -> map (ZEQ pkg) (toVersion registryIndex pkg v)
  ZOR cs -> map ZOR $ traverse (convertToVersion registryIndex) cs
  ZAND cs -> map ZAND $ traverse (convertToVersion registryIndex) cs
  ZVAR v -> pure (ZVAR v)
  ZTRU -> pure ZTRU
  ZIMP lhs rhs -> do
    l <- convertToVersion registryIndex lhs
    r <- convertToVersion registryIndex rhs
    pure $ ZIMP l r
  ZIFF l rhs -> do
    r <- convertToVersion registryIndex rhs
    pure $ ZIFF l r

getNames :: forall a. Z3Clause a -> Set VarDecl
getNames = case _ of
  ZGE pkg _ -> intVar pkg
  ZLT pkg _ -> intVar pkg
  ZEQ pkg _ -> intVar pkg
  ZOR cs -> Set.unions (map getNames cs)
  ZAND cs -> Set.unions (map getNames cs)
  ZIMP l r -> Set.unions [ getNames l, getNames r ]
  ZIFF l r -> Set.unions [ boolVar l, getNames r ]
  ZVAR v -> boolVar v
  ZTRU -> Set.empty
  where
  intVar pkg = Set.singleton { typ: "int", name: PackageName.print pkg }
  boolVar (Var var) = Set.singleton { typ: "bool", name: var }

unsafeLookup :: forall k v. Ord k => Show k => Show v => String -> k -> Map k v -> v
unsafeLookup canary k m = case Map.lookup k m of
  Just a -> a
  Nothing -> unsafeCrashWith $ canary <> ": tried to lookup key " <> show k <> " from map: " <> show m

unsafePackageName :: String -> PackageName
unsafePackageName = right <<< PackageName.parse
