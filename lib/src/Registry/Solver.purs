module Registry.Solver where

import Registry.Prelude

import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.App (App(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.List.NonEmpty as NEL
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Newtype (over, unwrap, wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Semigroup.Generic (genericAppend)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.TraversableWithIndex (traverseWithIndex)
import Registry.Json (decode, encode)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version, bumpPatch)
import Registry.Version as Version
import Safe.Coerce (coerce)

-- Prune the tree based on compiler version
-- but compiler doesn't have ranges yet
-- plan: https://github.com/haskell-CI/hackage-matrix-builder

data LocalSolverPosition
  = Trial
  | Root
  | Solving
    (NonEmptySet
      { package :: PackageName
      , version :: Version
      }
    )
derive instance Generic LocalSolverPosition _
derive instance Eq LocalSolverPosition
instance Show LocalSolverPosition where show = genericShow
instance RegistryJson LocalSolverPosition where
  encode Trial = encode "trial"
  encode Root = encode "root"
  encode (Solving stuff) = encode $ Array.fromFoldable stuff
  decode _ = Left "sorry"

instance Semigroup LocalSolverPosition where
  append Trial _ = Trial
  append _ Trial = Trial
  append Root _ = Root
  append _ Root = Root
  append (Solving r1) (Solving r2) = Solving (r1 <> r2)

data SolverPosition = Pos LocalSolverPosition (Set PackageName)
derive instance Generic SolverPosition _
derive instance Eq SolverPosition
instance Show SolverPosition where show = genericShow
instance RegistryJson SolverPosition where
  encode (Pos local global) = encode { local, global: Array.fromFoldable global }
  decode _ = Left "soz"

instance Semigroup SolverPosition where append = genericAppend

dependency :: SolverPosition -> SolverPosition -> SolverPosition
dependency (Pos _ g1) (Pos l2 g2) = Pos l2 (g1 <> g2)

dependencyOf :: forall z. Newtype z Sourced => SolverPosition -> z -> z
dependencyOf p1 = coerce \(Sourced v p2) ->
  Sourced v (dependency p1 p2)

asDependencyOf :: Intersection -> Intersection -> Intersection
asDependencyOf (Intersection i1) (Intersection i2) =
  let pos = getPos i1.lower <> getPos i1.upper
  in Intersection
    { lower: dependencyOf pos i2.lower
    , upper: dependencyOf pos i2.upper
    }

data DependencyFrom
  = DependencyFrom PackageName (Either Range Version)

data Sourced = Sourced Version SolverPosition
derive instance Eq Sourced
derive instance Generic Sourced _
instance RegistryJson Sourced where
  encode (Sourced version position) = encode { version, position }
  decode = decode >>> map
    \(r :: { version :: Version, position :: SolverPosition }) ->
      Sourced r.version r.position
instance Show Sourced where show = genericShow

unSource :: Sourced -> Version
unSource (Sourced v _) = v

newtype MinSourced = MinSourced Sourced
derive instance Newtype MinSourced _
derive newtype instance Eq MinSourced
derive newtype instance Show MinSourced
derive newtype instance RegistryJson MinSourced
instance Semigroup MinSourced where
  append a@(MinSourced (Sourced av as)) b@(MinSourced (Sourced bv bs)) =
    case compare av bv of
      LT -> a
      GT -> b
      EQ -> MinSourced (Sourced av (as <> bs))
newtype MaxSourced = MaxSourced Sourced
derive instance Newtype MaxSourced _
derive newtype instance Eq MaxSourced
derive newtype instance Show MaxSourced
derive newtype instance RegistryJson MaxSourced
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
derive newtype instance Eq Intersection
derive newtype instance Semigroup Intersection
derive newtype instance Show Intersection
derive newtype instance RegistryJson Intersection


-- Can be treated as a simple range by outside observers
-- but contains lower bounds for upper bounds …
newtype Loose
  = Loose
    { lower :: MinSourced
    , upper :: MaxSourced
    }
derive instance Newtype Loose _
derive newtype instance Show Loose

upperBound :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype y Sourced => r -> Version
upperBound = unwrap >>> _.upper >>> unwrap >>> \(Sourced v _) -> v

lowerBound :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => r -> Version
lowerBound = unwrap >>> _.lower >>> unwrap >>> \(Sourced v _) -> v

getPos :: forall z. Newtype z Sourced => z -> SolverPosition
getPos = unwrap >>> \(Sourced _ pos) -> pos

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
  { lower: MaxSourced (Sourced v (Pos Trial Set.empty))
  , upper: MinSourced (Sourced (bumpPatch v) (Pos Trial Set.empty))
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
          Tuple seen (SemigroupMap (asDependencyOf range <<< fromLoose <$> reqs))

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

type SolverErrors = NEL.NonEmptyList SolverError
data SolverError
  = Conflicts (Map PackageName Intersection)
  | WhileSolving PackageName (Map Version SolverError)

derive instance Eq SolverError
instance RegistryJson SolverError where
  encode = Json.fromString <<< printSolverError
  decode _ = Left "sorry cannot decode"

checkRequired ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either (Map PackageName Intersection) Unit
checkRequired { registry, required: SemigroupMap required } =
  if not Map.isEmpty failed then Left failed else pure unit
  where
  invalid package range = Map.isEmpty $
    getPackageRange registry package range
  failed = Map.filterWithKey invalid required

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

newtype LastSuccess b a = LastSuccess (Unit -> Either a b)
derive instance Newtype (LastSuccess b a) _
instance Functor (LastSuccess b) where
  map f = over LastSuccess (map (lmap f))
instance Apply (LastSuccess b) where
  apply (LastSuccess mf) (LastSuccess ma) = LastSuccess \u ->
    case ma u of
      Right v -> Right v
      Left a ->
        case mf u of
          Right v -> Right v
          Left f -> Left (f a)
instance Applicative (LastSuccess b) where
  pure = LastSuccess <<< pure <<< Left

solveFull ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either SolverErrors (Map PackageName Version)
solveFull = solveAux false
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
          sols = mapWithIndex (\version deps -> LastSuccess \_ -> solvePackage r package version deps) versions
        in case unwrap (sequence sols) unit of
          Right solved -> Right solved
          Left errors ->
            let
              err = WhileSolving package (NEL.head <$> errors)
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

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  Pos Root _ -> " (declared dependency)"
  Pos Trial _ -> " (attempted version)"
  Pos (Solving local) global ->
    " seen in " <> intercalateMap ", " printPackageVersion local
    <> case NEA.fromFoldable (Set.difference global (Set.map _.package (NES.toSet local))) of
      Nothing -> mempty
      Just as -> " from declared dependencies " <> intercalateMap ", " show as

printPackageVersion ::
  { package :: PackageName
  , version :: Version
  }
  -> String
printPackageVersion { package, version } =
  PackageName.print package <> "@" <> Version.printVersion version

derive instance Generic.Generic SolverError _
instance Show SolverError where show a = genericShow a

printSolverError :: SolverError -> String
printSolverError = printErrorAt ""

printErrorAt :: String -> SolverError -> String
printErrorAt indent = case _ of
  Conflicts conflicts -> intercalate ("\n" <> indent) $
    mapWithIndex (printConflict indent) conflicts
  WhileSolving package versions -> Array.fold
    [ "While solving "
    , PackageName.print package
    , " each version could not be solved:"
    , fold $ versions # mapWithIndex
        \version nested -> Array.fold
          [ "\n"
          , indent
          , "- "
          , Version.printVersion version
          , ": "
          , "\n"
          , indent <> "  "
          , printErrorAt (indent <> "  ") nested
          ]
    ]

printSourced :: forall i. Newtype i Sourced => i -> String
printSourced = unwrap >>> \(Sourced v pos) ->
  Version.printVersion v <> printSolverPosition pos

printConflict :: String -> PackageName -> Intersection -> String
printConflict indent package range | lowerBound range >= upperBound range = Array.fold
  [ "Conflict in version ranges for "
  , PackageName.print package
  , ":"
  , "\n", indent, "  >=", printSourced (unwrap range).lower
  , "\n", indent, "  <",  printSourced (unwrap range).upper
  ]
printConflict indent package range = Array.fold
  [ "No versions found in the registry for "
  , PackageName.print package
  , " in range"
  , "\n", indent, "  >=", printSourced (unwrap range).lower
  , "\n", indent, "  <",  printSourced (unwrap range).upper
  ]

{-
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
-}

intersectionFromRange :: PackageName -> Version -> Range -> Intersection
intersectionFromRange package version range =
  let
    mkSourced v = Sourced v $ Pos (Solving (NES.singleton { package, version })) Set.empty
  in Intersection
    { lower: wrap $ mkSourced (Version.greaterThanOrEq range)
    , upper: wrap $ mkSourced (Version.lessThan range)
    }

intersectionFromRange' :: PackageName -> Range -> Intersection
intersectionFromRange' package range =
  let
    mkSourced v = Sourced v (Pos Root (Set.singleton package))
  in Intersection
    { lower: wrap $ mkSourced (Version.greaterThanOrEq range)
    , upper: wrap $ mkSourced (Version.lessThan range)
    }

solve :: Dependencies -> Map PackageName Range -> Either (NEL.NonEmptyList SolverError) (Map PackageName Version)
solve index pending =
  let
    registry = mapWithIndex (\package -> mapWithIndex \version -> map (intersectionFromRange package version)) $ coerce index
  in solve' registry pending

solve' :: TransitivizedRegistry -> Map PackageName Range -> Either (NEL.NonEmptyList SolverError) (Map PackageName Version)
solve' registry pending =
  let
    required = mapWithIndex intersectionFromRange' $ coerce pending
  in case solveFull { registry, required } of
    Left e -> Left e
    Right r -> Right r
