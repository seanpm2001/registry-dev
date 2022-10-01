module Registry.Solver where

import Registry.Prelude

import Control.Apply (lift2)
import Control.Comonad (extract)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (allWithIndex, foldMapWithIndex, traverseWithIndex_)
import Data.Function (on)
import Data.Functor.App (App(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep as Generic
import Data.List.NonEmpty as NEL
import Data.List.Types (List(..), NonEmptyList(..), (:))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Newtype (alaF, over, unwrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Ord.Min (Min(..))
import Data.Profunctor (dimap)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.These (These(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version, bumpPatch, intersect, printRange, rangeIncludes)
import Registry.Version as Version
import Safe.Coerce (coerce)
import Uncurried.RWSE (RWSE, runRWSE)

-- Prune the tree based on compiler version
-- but compiler doesn't have ranges yet
-- plan: https://github.com/haskell-CI/hackage-matrix-builder

data SolverPositionS
  = SolveRoot0
  | Trial
  | Solving1
    { depth :: Min Int
    , local :: Set
      { package :: PackageName
      , version :: Version
      }
    , global :: Set PackageName
    }

instance Semigroup SolverPositionS where
  append SolveRoot0 _ = SolveRoot0
  append _ SolveRoot0 = SolveRoot0
  append (Solving1 r1) (Solving1 r2) = Solving1 (r1 <> r2)
  append Trial _ = Trial
  append _ Trial = Trial

data DependencyFrom
  = DependencyFrom PackageName (Either Range Version)

data Sourced = Sourced Version SolverPositionS

unSource :: Sourced -> Version
unSource (Sourced v _) = v

newtype MinSourced = MinSourced Sourced
derive instance Newtype MinSourced _
instance Semigroup MinSourced where
  append a@(MinSourced (Sourced av as)) b@(MinSourced (Sourced bv bs)) =
    case compare av bv of
      LT -> a
      GT -> b
      EQ -> MinSourced (Sourced av (as <> bs))
newtype MaxSourced = MaxSourced Sourced
derive instance Newtype MaxSourced _
instance Semigroup MaxSourced where
  append a@(MaxSourced (Sourced av as)) b@(MaxSourced (Sourced bv bs)) =
    case compare av bv of
      GT -> a
      LT -> b
      EQ -> MaxSourced (Sourced av (as <> bs))

newtype Intersection
  = Intersection
    { lower :: MaxSourced
    , upper :: MinSourced
    }
derive instance Newtype Intersection _

data Pref = Pref Loose | PrefLR | PrefRL

derive newtype instance Semigroup Intersection

-- Can be treated as a simple range by outside observers
-- but contains lower bounds for upper bounds …
newtype Loose
  = Loose
    { lower :: MinSourced
    , upper :: MaxSourced
    }
derive instance Newtype Loose _

upperBound :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => r -> Version
upperBound = unwrap >>> _.lower >>> unwrap >>> \(Sourced v _) -> v

lowerBound :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype y Sourced => r -> Version
lowerBound = unwrap >>> _.upper >>> unwrap >>> \(Sourced v _) -> v

derive newtype instance Semigroup Loose

toLoose :: Intersection -> Maybe Loose
toLoose r | lowerBound r < upperBound r = Just (coerce r)
toLoose _ = Nothing

fromLoose :: Loose -> Intersection
fromLoose = coerce

commonalities ::
  NonEmptyArray (Map PackageName { range :: Loose, seen :: Set Version }) ->
  { required :: App (Map PackageName) Loose
  , seen :: SemigroupMap PackageName (Set Version)
  }
commonalities = NEA.foldMap1 \deps ->
  { required: App (deps <#> _.range)
  , seen: SemigroupMap (deps <#> _.seen)
  }

satisfies :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => Newtype y Sourced =>
  Version -> r -> Boolean
satisfies v r = v >= lowerBound r && v < upperBound r

getPackageRange :: forall r x y d.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => Newtype y Sourced =>

  SemigroupMap PackageName (SemigroupMap Version d) ->
  PackageName -> r -> Map Version d
getPackageRange (SemigroupMap registry) package range =
  case Map.lookup package registry of
    Nothing -> Map.empty
    Just (SemigroupMap versions) ->
      Map.filterKeys (\v -> v `satisfies` range) versions

soleVersion :: Version -> Intersection
soleVersion v = Intersection
  { lower: MaxSourced (Sourced v Trial)
  , upper: MinSourced (Sourced (bumpPatch v) Trial)
  }

soleVersionOf :: PackageName -> Version -> SemigroupMap PackageName Intersection
soleVersionOf package v = SemigroupMap (Map.singleton package (soleVersion v))

type TransitivizedRegistry =
  SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName Intersection))

-- We record what dependencies are required no matter which path we take,
-- and what package versions we saw to explore next (which is basically the
-- same, but not necessarily contiguous and does not get removed if
-- one version does not have a dependency on that package)
commonDependencies ::
  TransitivizedRegistry ->
  PackageName -> Intersection ->
  SeenWith (SemigroupMap PackageName Intersection)
commonDependencies registry package range =
  let
    inRange =
      getPackageRange registry package range
    solvableInRange =
      Array.mapMaybe (traverse toLoose) (Array.fromFoldable inRange)
    augment packageDep (rangeDep :: Loose) =
      { range: rangeDep
      , seen: Map.keys (getPackageRange registry packageDep rangeDep)
      }
  in case NEA.fromArray solvableInRange of
    Nothing -> mempty
    Just versionDependencies ->
      case commonalities (mapWithIndex augment <<< un SemigroupMap <$> versionDependencies) of
        { required: App reqs, seen } ->
          Tuple seen (SemigroupMap (fromLoose <$> reqs))

type Seen = SemigroupMap PackageName (Set Version)
type SeenWith = Tuple Seen
type SeenWithRegistry = SeenWith TransitivizedRegistry

exploreTransitiveDependencies ::
  SeenWithRegistry -> SeenWithRegistry
exploreTransitiveDependencies (Tuple (SemigroupMap seen) registry) =
  traverseWithIndex (traverseWithIndex <<< doTheThing) registry
  where
    doTheThing package version
      | Just versions <- Map.lookup package seen
      , Set.member version versions
      = pure <> foldMapWithIndex (commonDependencies registry)
      | otherwise = pure


solveStep ::
  { required :: SemigroupMap PackageName Intersection
  , registry :: TransitivizedRegistry
  , seen :: Seen
  } ->
  { required :: SemigroupMap PackageName Intersection
  , registry :: TransitivizedRegistry
  , seen :: Seen
  }
solveStep initial = base <> more
  where
  base = { required: initial.required, registry: initial.registry, seen: mempty }
  Tuple seenHere moreRequired = initial.required
    # foldMapWithIndex (commonDependencies initial.registry)
  Tuple seenDeeper moreRegistry = exploreTransitiveDependencies
    (Tuple initial.seen initial.registry)
  more = { required: moreRequired, registry: moreRegistry, seen: seenHere <> seenDeeper }

fixEq :: forall a. Eq a => (a -> a) -> (a -> a)
fixEq f a = let b = f a in if b == a then a else fixEq f b

solveSteps ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  , seen :: SemigroupMap PackageName (Set Version)
  } ->
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  , seen :: SemigroupMap PackageName (Set Version)
  }
solveSteps a =
  let
    unSourceI :: Intersection -> { lower :: Version, upper :: Version }
    unSourceI (Intersection { lower: MaxSourced (Sourced lower _), upper: MinSourced (Sourced upper _) }) =
      { lower, upper }
    prj ::
      { required :: SemigroupMap PackageName Intersection
      , registry :: TransitivizedRegistry
      , seen :: Seen
      } ->
      { required :: SemigroupMap PackageName { lower :: Version, upper :: Version }
      , registry :: SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName { lower :: Version, upper :: Version }))
      , seen :: Seen
      }
    prj r =
      { required: map unSourceI r.required
      , registry: (map <<< map <<< map) unSourceI r.registry
      , seen: r.seen
      }
    b = solveStep a
  in if prj a == prj b then a else solveSteps b

type NewSolverErrors = NEL.NonEmptyList NewSolverError
data NewSolverError
  = Conflicts (Map PackageName Intersection)
  | WhileSolving PackageName (Map Version NewSolverError)

checkRequired ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either (Map PackageName Intersection) Unit
checkRequired { registry, required: SemigroupMap required } =
  if not Map.isEmpty failed then Left failed else pure unit
  where
  stillValid package range = not Map.isEmpty $
    getPackageRange registry package range
  failed = Map.filterWithKey stillValid required

checkSolved ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either
    { package :: PackageName, versions :: Map Version (SemigroupMap PackageName Intersection) }
    (Map PackageName Version)
checkSolved { registry, required: SemigroupMap required } =
  required # traverseWithIndex \package range ->
    let filteredVersions = getPackageRange registry package range in
    case Map.size filteredVersions, Map.findMax filteredVersions of
      1, Just { key: version } -> pure version
      _, _ -> Left { package, versions: filteredVersions }

solveFull ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either NewSolverErrors (Map PackageName Version)
solveFull = solveAux false -- true -- TODO
  where
  solveAux continue { registry, required } = do
    let
      r = case solveSteps { registry, required, seen: mempty } of
        r' -> { registry: r'.registry, required: r'.required }
    lmap (pure <<< Conflicts) (checkRequired r)
    case checkSolved r of
      Right solved -> Right solved
      Left { package, versions } ->
        let
          sols = mapWithIndex (solvePackage r package) versions
        in case traverse (either Right Left) sols of
          Left solved -> Right solved
          Right errors ->
            let
              err = WhileSolving package (extract <$> errors)
              errs = if not continue then mempty else
                case solveAux continue (package `deleteFrom` r) of
                  Left more -> NEL.toList more
                  Right _ -> mempty
            in Left $ NEL.cons' err errs
  solvePackage r package version dependencies =
    let required = r.required <> soleVersionOf package version <> dependencies
    in solveAux false { registry: r.registry, required }
  deleteFrom package { registry, required } = do
    let
      deleter :: forall v. SemigroupMap PackageName v -> SemigroupMap PackageName v
      deleter = over SemigroupMap (Map.delete package)
    { required: deleter required
    , registry: map deleter <$> deleter registry
    }

type Dependencies = Map PackageName (Map Version (Map PackageName Range))

data SolverPosition
  = SolveRoot
  | Solving PackageName (NonEmptyArray Version) SolverPosition

derive instance Eq SolverPosition
derive instance Ord SolverPosition
derive instance Generic.Generic SolverPosition _

instance Show SolverPosition where
  show a = genericShow a

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  SolveRoot -> ""
  Solving name versions pos -> Array.fold
    [ " while solving "
    , PackageName.print name
    , "@"
    , intercalateMap ", " Version.printVersion versions
    , printSolverPosition pos
    ]

-- Find the shortest path from the initial dependencies to the desired specific
-- dependencies (package@version)
minimizeSolverPositions :: Dependencies -> Map PackageName Range -> Map PackageName (Set Version) -> Map (Tuple PackageName Version) (First SolverPosition)
minimizeSolverPositions index initialGoals errored = go Map.empty (map (NEA.singleton <<< Tuple SolveRoot) initialGoals)
  where
  toBeFound :: Set (Tuple PackageName Version)
  toBeFound = errored # foldMapWithIndex (Set.map <<< Tuple)

  go
    :: Map (Tuple PackageName Version) (First SolverPosition)
    -> Map PackageName (NonEmptyArray (Tuple SolverPosition Range))
    -> Map (Tuple PackageName Version) (First SolverPosition)
  -- Found enough, go home
  go found _ | toBeFound `Set.subset` Map.keys found = found
  -- Add relevant things from `currentGoals`, go to next layer
  go alreadyFound currentGoals =
    let
      foundHere = currentGoals
        # foldMapWithIndex \package -> foldMap \(Tuple pos range) ->
            Map.lookup package errored
              # foldMap (Set.filter (Version.rangeIncludes range))
              # foldMap \version -> Map.singleton (Tuple package version) (First pos)

      nextGoals :: Map PackageName (NonEmptyArray (Tuple SolverPosition Range))
      nextGoals = currentGoals
        # foldMapWithIndex \package -> foldMap \(Tuple pos range) ->
            Map.lookup package index
              # maybe Map.empty (Map.filterWithKey (\v _ -> Version.rangeIncludes range v))
              # foldMapWithIndex \version deps ->
                  NEA.singleton <<< Tuple (Solving package (pure version) pos) <$> deps
    in
      go (alreadyFound <> foundHere) nextGoals

data SolverError
  = NoVersionsInRange PackageName (Set Version) Range SolverPosition
  | VersionNotInRange PackageName (NonEmptySet Version) Range SolverPosition
  | DisjointRanges PackageName Range SolverPosition Range SolverPosition

derive instance Eq SolverError
derive instance Generic.Generic SolverError _

instance Show SolverError where
  show a = genericShow a

-- Minimize the positions on the errors and group/deduplicate them
minimizeErrors :: Dependencies -> Map PackageName Range -> NonEmptyArray SolverError -> NonEmptyArray SolverError
minimizeErrors index goals errs = groupErrors (minimizePosition =<< errs)
  where
  collected = errs # foldMap case _ of
    NoVersionsInRange _ _ _ pos -> collectPackageVersion pos
    VersionNotInRange _ _ _ pos -> collectPackageVersion pos
    DisjointRanges _ _ p1 _ p2 ->
      collectPackageVersion p1 <> collectPackageVersion p2

  collectPackageVersion SolveRoot = mempty
  collectPackageVersion (Solving package version _) =
    foldMap (Map.singleton package <<< Set.singleton) version

  minimizePosition (NoVersionsInRange a b c pos) = NoVersionsInRange a b c <$> getMinimized pos
  minimizePosition (VersionNotInRange a b c pos) = VersionNotInRange a b c <$> getMinimized pos
  minimizePosition (DisjointRanges a b p1 c p2) = DisjointRanges a b <$> getMinimized p1 <@> c <*> getMinimized p2

  minimized = minimizeSolverPositions index goals collected

  lookupMinimized package versions = map sequence $ versions # traverse
    \version -> Map.lookup (Tuple package version) minimized
  getMinimized (Solving package versions _)
    | Just (First pos) <- lookupMinimized package versions = Solving package versions <$> NEA.nub pos
  getMinimized pos = pure pos

groupErrors :: NonEmptyArray SolverError -> NonEmptyArray SolverError
groupErrors = compose groupErrors2 $ fromGroup <=< NEA.groupAllBy grouping
  where
  grouping (NoVersionsInRange p1 v1 r1 _) (NoVersionsInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (printRange r1) (printRange r2)
  grouping (NoVersionsInRange _ _ _ _) _ = LT
  grouping _ (NoVersionsInRange _ _ _ _) = GT
  grouping (VersionNotInRange p1 v1 r1 _) (VersionNotInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (printRange r1) (printRange r2)
  grouping (VersionNotInRange _ _ _ _) _ = LT
  grouping _ (VersionNotInRange _ _ _ _) = GT
  grouping (DisjointRanges p1 r1 s1 q1 _) (DisjointRanges p2 r2 s2 q2 _) =
    compare p1 p2 <> compare (printRange r1) (printRange r2) <> compare s1 s2 <> compare (printRange q1) (printRange q2)

  fromGroup es = setPosition (NEA.head es) $ groupPositions $ map getPosition es

  getPosition (NoVersionsInRange _ _ _ p) = p
  getPosition (VersionNotInRange _ _ _ p) = p
  getPosition (DisjointRanges _ _ _ _ p) = p

  setPosition (NoVersionsInRange p v r _) = map $ NoVersionsInRange p v r
  setPosition (VersionNotInRange p v r _) = map $ VersionNotInRange p v r
  setPosition (DisjointRanges p r s q _) = map $ DisjointRanges p r s q

groupErrors2 :: NonEmptyArray SolverError -> NonEmptyArray SolverError
groupErrors2 = map fromGroup <<< NEA.groupAllBy grouping
  where
  grouping (VersionNotInRange p1 _ r1 t1) (VersionNotInRange p2 _ r2 t2) =
    compare p1 p2 <> compare (printRange r1) (printRange r2) <> compare t1 t2
  grouping (VersionNotInRange _ _ _ _) _ = LT
  grouping _ (VersionNotInRange _ _ _ _) = GT
  grouping (NoVersionsInRange p1 v1 r1 q1) (NoVersionsInRange p2 v2 r2 q2) =
    compare p1 p2 <> compare v1 v2 <> compare (printRange r1) (printRange r2) <> compare q1 q2
  grouping (NoVersionsInRange _ _ _ _) _ = LT
  grouping _ (NoVersionsInRange _ _ _ _) = GT
  grouping (DisjointRanges p1 r1 s1 q1 t1) (DisjointRanges p2 r2 s2 q2 t2) =
    compare p1 p2 <> compare (printRange r1) (printRange r2) <> compare s1 s2 <> compare (printRange q1) (printRange q2) <> compare t1 t2

  fromGroup :: NonEmptyArray SolverError -> SolverError
  fromGroup es = setVersions (NEA.head es) $ foldMap1 getVersion es

  setVersions (VersionNotInRange p _ r q) (Just vs) = VersionNotInRange p vs r q
  setVersions e _ = e
  getVersion (VersionNotInRange _ vs _ _) = Just vs
  getVersion _ = Nothing

groupPositions :: NonEmptyArray SolverPosition -> NonEmptyArray SolverPosition
groupPositions = fromGroup <=< NEA.groupAllBy grouping
  where
  grouping SolveRoot SolveRoot = EQ
  grouping SolveRoot _ = LT
  grouping _ SolveRoot = GT
  grouping (Solving p1 _ s1) (Solving p2 _ s2) =
    compare p1 p2 <> compare s1 s2

  fromGroup es = setVersion es $ Array.nub $ getVersions =<< NEA.toArray es

  getVersions SolveRoot = empty
  getVersions (Solving _ v _) = NEA.toArray v

  setVersion os = NEA.fromArray >>>
    maybe os case NEA.head os, _ of
      SolveRoot, _ -> pure SolveRoot
      Solving p _ s, v -> pure (Solving p v s)

printSolverError :: SolverError -> String
printSolverError = case _ of
  NoVersionsInRange name versions range pos -> Array.fold
    [ "Package index contained no versions for "
    , PackageName.print name
    , " in the range "
    , Version.printRange range
    , " (existing versions: "
    , maybe "none" (intercalateMap ", " Version.printVersion) (NEA.fromFoldable versions)
    , ")"
    , printSolverPosition pos
    ]
  VersionNotInRange name version range pos -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , "@"
    , intercalateMap ", " Version.printVersion version
    , " but the range "
    , Version.printRange range
    , " was also required"
    , printSolverPosition pos
    ]
  DisjointRanges name range1 pos1 range2 pos2 -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , " in range "
    , Version.printRange range1
    , printSolverPosition pos1
    , " but the range "
    , Version.printRange range2
    , " was also required"
    , printSolverPosition pos2
    ]

type Solver = RWSE Dependencies Unit State (NonEmptyArray SolverError)

type Goals = Map PackageName (Tuple SolverPosition Range)
type Solved = Map PackageName Version
type State =
  { pending :: Goals
  , solved :: Map PackageName Version
  }

newtype CollectErrors :: Type -> Type
newtype CollectErrors a = CollectErrors (Solver a)

derive instance Newtype (CollectErrors a) _

instance Semigroup (CollectErrors a) where
  append (CollectErrors fa) (CollectErrors fb) = CollectErrors do
    s <- get
    catchError fa \e1 -> do
      put s
      catchError fb \e2 -> do
        throwError (groupErrors $ e1 <> e2)

oneOfMap1 :: forall a b. (a -> Solver b) -> NonEmptyArray a -> Solver b
oneOfMap1 = alaF CollectErrors foldMap1

type ValidationError =
  { name :: PackageName
  , range :: Range
  , version :: Maybe Version
  }

validate :: Map PackageName Range -> Solved -> Either (NonEmptyArray ValidationError) Unit
validate index sols = maybe (Right unit) Left $ NEA.fromArray
  $ index
  # foldMapWithIndex \name range ->
      case Map.lookup name sols of
        Just version | rangeIncludes range version -> empty
        version -> pure { name, range, version }

solve :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solve index pending = lmap (minimizeErrors index pending)
  case runRWSE index { pending: map (Tuple SolveRoot) pending, solved: Map.empty } (exploreGoals 20) of
    _ /\ r /\ _ -> r

solveAndValidate :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solveAndValidate index pending = do
  sols <- solve index pending
  case validate pending sols of
    Left es -> Left $ es <#> \r ->
      case r.version of
        Nothing -> NoVersionsInRange r.name Set.empty r.range SolveRoot
        Just version -> VersionNotInRange r.name (NES.singleton version) r.range SolveRoot
    Right _ -> Right sols

exploreGoals :: Int -> Solver Solved
exploreGoals work =
  get >>= \goals@{ pending, solved } ->
    case Map.findMin pending of
      Nothing ->
        pure solved

      Just { key: name, value: Tuple pos constraint } -> do
        let otherPending = Map.delete name pending
        let goals' = goals { pending = otherPending }
        put goals'
        versions <- getRelevantVersions pos name constraint
        let
          act = versions # oneOfMap1 \version ->
            addVersion pos name version *> exploreGoals 0
        catchError act \e1 -> do
          when (work > 0) $ void do
            put goals'
            catchError (exploreGoals (work - NEA.length e1)) \e2 -> do
              throwError (e1 <> e2)
          throwError e1

addVersion :: SolverPosition -> PackageName -> (Tuple Version (Map PackageName Range)) -> Solver Unit
addVersion pos name (Tuple version deps) = do
  modify_ \s -> s { solved = Map.insert name version s.solved }
  traverseWithIndex_ (addConstraint (Solving name (pure version) pos)) deps

addConstraint :: SolverPosition -> PackageName -> Range -> Solver Unit
addConstraint pos name newConstraint = do
  goals@{ pending, solved } <- get
  case Map.lookup name solved of
    Just version ->
      if rangeIncludes newConstraint version then pure unit
      else throwError $ pure $ VersionNotInRange name (NES.singleton version) newConstraint pos

    Nothing ->
      case Map.lookup name pending of
        Nothing -> put $ goals { pending = Map.insert name (Tuple pos newConstraint) pending }

        Just (Tuple oldPos oldConstraint) ->
          case intersect oldConstraint newConstraint of
            Nothing ->
              throwError $ pure $ DisjointRanges name oldConstraint oldPos newConstraint pos

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint then pure unit
              else put $ goals { pending = Map.insert name (Tuple pos mergedConstraint) pending }

getRelevantVersions :: SolverPosition -> PackageName -> Range -> Solver (NonEmptyArray (Tuple Version (Map PackageName Range)))
getRelevantVersions pos name constraint = do
  index <- ask
  let
    versions =
      Map.lookup name index # foldMap do
        -- Put newest versions first
        Map.toUnfoldable >>> Array.reverse
          >>> Array.filter (fst >>> rangeIncludes constraint)
          >>> NEA.fromArray
  case versions of
    Just vs -> pure vs
    Nothing ->
      throwError $ pure $ NoVersionsInRange name (Map.lookup name index # foldMap Map.keys) constraint pos
