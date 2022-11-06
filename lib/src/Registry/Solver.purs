module Registry.Solver where

import Registry.Prelude

import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap, intercalate, sum)
import Data.FoldableWithIndex (anyWithIndex, foldMapWithIndex)
import Data.Functor.App (App(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.List.NonEmpty as NEL
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Monoid (power)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (over, unwrap, wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.String as String
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Class.Console (time, timeEnd)
import Effect.Unsafe (unsafePerformEffect)
import Registry.Json (decode, encode, printJson)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version, bumpPatch)
import Registry.Version as Version
import Safe.Coerce (coerce)

-- Prune the tree based on compiler version
-- but compiler doesn't have ranges yet
-- plan: https://github.com/haskell-CI/hackage-matrix-builder

data LocalSolverPosition
  -- | Dependency asked for in manifest
  = Root
  -- | Committed to a specific version
  | Trial
  -- | Required transitive dependency seen in said packages
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

instance Semigroup SolverPosition where
  append (Pos l1 g1) (Pos l2 g2) =
    Pos (l1 <> l2) (g1 <> g2)

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

good :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => Newtype y Sourced => r -> Boolean
good i = upperBound i > lowerBound i

getPos :: forall z. Newtype z Sourced => z -> SolverPosition
getPos = unwrap >>> \(Sourced _ pos) -> pos

derive newtype instance Semigroup Loose

toLoose :: Intersection -> Maybe Loose
toLoose r | lowerBound r < upperBound r = Just (coerce r)
toLoose _ = Nothing

fromLoose :: Loose -> Intersection
fromLoose = coerce

satisfies :: forall r x y.
  Newtype r { lower :: x, upper :: y } =>
  Newtype x Sourced => Newtype y Sourced =>
  Version -> r -> Boolean
satisfies v r = v >= lowerBound r && v < upperBound r

getPackageRange :: forall range x y d.
  Newtype range { lower :: x, upper :: y } =>
  Newtype x Sourced => Newtype y Sourced =>

  SemigroupMap PackageName (SemigroupMap Version d) ->
  PackageName -> range -> SemigroupMap Version d
getPackageRange (SemigroupMap registry) package range =
  case Map.lookup package registry of
    Nothing -> SemigroupMap Map.empty
    Just (SemigroupMap versions) ->
      SemigroupMap $ Map.filterKeys (\v -> v `satisfies` range) versions

soleVersion :: Version -> Intersection
soleVersion v = Intersection
  { lower: MaxSourced (Sourced v (Pos Trial Set.empty))
  , upper: MinSourced (Sourced (bumpPatch v) (Pos Trial Set.empty))
  }

soleVersionOf :: PackageName -> Version -> SemigroupMap PackageName Intersection
soleVersionOf package v = SemigroupMap (Map.singleton package (soleVersion v))

type TransitivizedRegistry =
  SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName Intersection))
type RR r =
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  | r
  }

gatherReachable :: forall r. RR r -> TransitivizedRegistry
gatherReachable { registry, required } =
  let
    reachable0 :: SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName Intersection))
    reachable0 = mapWithIndex (getPackageRange registry) required
    reachable' = identity <> (foldMap <<< foldMap) (mapWithIndex (getPackageRange registry))
    reachable = fixEq reachable' reachable0
  in trace [show (Map.size (unwrap reachable)), " reachable packages with ", show (sum (Map.size <<< unwrap <$> reachable)), " versions"] reachable

withReachable :: forall r. RR r -> RR r
withReachable r = r { registry = map (_ <> r.required) <$> gatherReachable r }

trimReachable :: forall r. RR r -> RR r
trimReachable r = r
  { registry = r.registry # mapWithIndex \package ->
      over SemigroupMap $ Map.filterWithKey \version _ ->
        case Map.lookup package $ unwrap r.required of
          Nothing -> true
          Just range -> satisfies version range
  }

-- We record what dependencies are required no matter which path we take,
-- and what package versions we saw to explore next (which is basically the
-- same, but not necessarily contiguous and does not get removed if
-- one version does not have a dependency on that package)
commonDependencies ::
  TransitivizedRegistry ->
  PackageName -> Intersection ->
  SemigroupMap PackageName Intersection
commonDependencies registry package range =
  let
    inRange =
      getPackageRange registry package range
    solvableInRange =
      Array.mapMaybe (traverse toLoose) (Array.fromFoldable inRange)
  in case NEA.fromArray solvableInRange of
    Nothing -> mempty
    Just versionDependencies ->
      case NEA.foldMap1 App (un SemigroupMap <$> versionDependencies) of
        App reqs ->
          SemigroupMap $ reqs <#> asDependencyOf range <<< fromLoose

noUpdates :: forall r k v. { updated :: SemigroupMap k v | r } -> Boolean
noUpdates { updated: SemigroupMap updated } = Map.isEmpty updated

exploreAllTransitiveDependencies :: TransitivizedRegistry -> TransitivizedRegistry
exploreAllTransitiveDependencies registry = go { registry, updated: registry }
  where
  go r | noUpdates r = r.registry
  go r = go (exploreTransitiveDependencies r)

exploreTransitiveDependencies :: forall r.
  { registry :: TransitivizedRegistry
  , updated :: TransitivizedRegistry
  | r
  } ->
  { registry :: TransitivizedRegistry
  , updated :: TransitivizedRegistry
  }
exploreTransitiveDependencies lastTick = (\t -> { updated: fst t, registry: snd t }) $
  lastTick.registry # traverseWithIndex \package -> traverseWithIndex \version deps ->
    let
      updateOne depName depRange = case Map.isEmpty (unwrap (getPackageRange lastTick.updated depName depRange)) of
        true -> mempty
        false -> Tuple (Disj true) (commonDependencies lastTick.registry depName depRange)
      Tuple (Disj peek) newDeps = foldMapWithIndex updateOne deps
      -- keep GC churn down by re-using old deps if nothing changed, maybe?
      dependencies = if peek then deps <> newDeps else deps
      updated = case peek && majorUpdate deps dependencies of
        true -> SemigroupMap $ Map.singleton package $ SemigroupMap $ Map.singleton version dependencies
        false -> mempty
    in Tuple updated dependencies

solveStep ::
  { required :: SemigroupMap PackageName Intersection
  , registry :: TransitivizedRegistry
  , updated :: TransitivizedRegistry
  } ->
  { required :: SemigroupMap PackageName Intersection
  , registry :: TransitivizedRegistry
  , updated :: TransitivizedRegistry
  }
solveStep initial =
  { required: initial.required <> moreRequired
  , registry: moreRegistry
  , updated: updated <> updatedOfReqs
  }
  where
  -- Transitivize direct requirements
  moreRequired = initial.required # foldMapWithIndex (commonDependencies initial.registry)
  -- Record updates to them
  updatedOfReqs = requirementUpdates initial moreRequired
  -- Transitivize the rest of the registry, which should be:
  --   (1) Pruned at the start to only reachable package versions
  --   (2) Only touching packages that were directly updated last round
  { updated, registry: moreRegistry } = exploreTransitiveDependencies (initial { registry = map (_ <> moreRequired) <$> initial.registry })

fixEq :: forall a. Eq a => (a -> a) -> (a -> a)
fixEq f a = let b = f a in if b == a then a else fixEq f b

-- A package may update because its dependencies tightened, but any reverse
-- dependencies should have already caught that update. So what we look for
-- is either a new transitive dependency picked up (which the parent will need
-- to incorporate) or newly failing to solve.
majorUpdate :: SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> Boolean
majorUpdate (SemigroupMap orig) updated =
  let
    minor = { added: false, failedAlready: false, failedNow: false }
    -- TODO: short-circuit on `added = true`??
    info :: { added :: Boolean, failedNow :: Boolean, failedAlready :: Boolean }
    info = updated # anyWithIndex \package range ->
      case Map.lookup package orig of
        Nothing -> minor { added = true }
        Just r -> minor { failedAlready = not good r, failedNow = not good range }
  in case info of
    { added: true } -> true
    { failedNow: true, failedAlready: false } -> true
    _ -> false

trace msg a = unsafePerformEffect (a <$ log (Array.fold msg))
spy lbl a = trace [lbl, ": ", show a] a
spyVia lbl manip a = trace [lbl, ": ", show (manip a)] a
showUpdates = Array.fromFoldable <<< Map.keys <<< unwrap

requirementUpdates :: forall r.
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  | r
  } -> SemigroupMap PackageName Intersection -> TransitivizedRegistry
requirementUpdates { registry: SemigroupMap registry, required: SemigroupMap required } =
  foldMapWithIndex \package newRange ->
    let
      changed =
        case Map.lookup package required of
          Nothing -> true
          Just oldRange ->
            lowerBound oldRange < lowerBound newRange ||
            upperBound oldRange > upperBound newRange
    in if not changed then mempty else
      case Map.lookup package registry of
        Just versions -> SemigroupMap $ Map.singleton package versions
        Nothing -> mempty


solveSeed ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  , updated :: TransitivizedRegistry
  }
solveSeed { registry, required } = { registry, required, updated: registry }

downcast :: Intersection -> _
downcast i | good i = Right $ ">=" <> Version.printVersion (lowerBound i) <> " <" <> Version.printVersion (upperBound i)
downcast i = Left $ "<" <> Version.printVersion (upperBound i) <> " >=" <> Version.printVersion (lowerBound i)

downcastR :: TransitivizedRegistry -> _
downcastR r = map map map (partitionMap downcast) $ map map map unwrap $ map unwrap $ unwrap r

downcastR' :: TransitivizedRegistry -> _
downcastR' = downcastR >>> map (Map.keys >>> Array.fromFoldable)

stuff :: TransitivizedRegistry -> String
stuff r =
  let
    txt1 = printJson $ downcastR r
  in if String.length txt1 < 16383 then txt1
    else printJson $ downcastR' r

solveSteps ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  , updated :: TransitivizedRegistry
  } ->
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  }
solveSteps r0 = go 0 r0 where
  go i r@{ registry, required } | noUpdates r =
    trace ["took ", show i, " iterations:"] { registry, required }
  go i r = go (i + 1) (solveStep r)

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
  invalid package range = Map.isEmpty $ unwrap $
    getPackageRange registry package range
  failed = Map.filterWithKey invalid required

getLatest ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Maybe (Map PackageName { version :: Version, dependencies :: SemigroupMap PackageName Intersection })
getLatest { registry, required: SemigroupMap required } =
  forWithIndex required \package range -> do
    let SemigroupMap possibilities = getPackageRange registry package range
    { key, value } <- Map.findMax possibilities
    pure { version: key, dependencies: value }

-- Try the latest available versions of each package. This is safe/optimal
-- because bounds only shrink as required, so if the latest bounds already
-- satisfy all of the requirements, those bounds won't ever need to shrink and
-- this is the solution we would find anyways.
tryLatest ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Maybe (Map PackageName Version)
tryLatest r = do
  sol <- getLatest r
  -- By construction this satisfies required, so we just
  -- need to check that each has its dependencies included
  for sol \{ version, dependencies } -> do
    forWithIndex_ dependencies \dep range -> do
      { version: vDep } <- Map.lookup dep sol
      guardA (satisfies vDep range)
    pure version

checkSolved ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either
    { package :: PackageName, versions :: Map Version (SemigroupMap PackageName Intersection) }
    (Map PackageName Version)
checkSolved r | Just solution <- tryLatest r = pure solution
checkSolved { registry, required: SemigroupMap required } =
  required # traverseWithIndex \package range ->
    let SemigroupMap filteredVersions = getPackageRange registry package range in
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

perf :: forall b. String -> (Unit -> b) -> b
perf name f = unsafePerformEffect do
  time name
  let b = f unit
  timeEnd name
  pure b
perf1 :: forall a b. String -> (a -> b) -> a -> b
perf1 name f a = perf name \_ -> f a

solveFull ::
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  } ->
  Either SolverErrors (Map PackageName Version)
solveFull = (perf1 "^^^ solveFull" (solveAux 0 false)) <<< solveSeed <<< (perf1 "withReachable0" withReachable)
  where
  solveAux ::
    Int ->
    Boolean ->
    { registry :: TransitivizedRegistry
    , updated :: TransitivizedRegistry
    , required :: SemigroupMap PackageName Intersection
    } ->
    Either SolverErrors (Map PackageName Version)
  solveAux i continue a = perf1 "solveSteps" solveSteps a # trace ["> solveAux", show i] (perf1 ("< solveAux" <> show i) \r -> do
    lmap (pure <<< Conflicts) (checkRequired r)
    case checkSolved r of
      Right solved -> Right solved
      Left { package, versions } ->
        let
          sols = mapWithIndex (\version deps -> LastSuccess \_ -> solvePackage i r package version deps) versions
        in case unwrap (sequence sols) unit of
          Right solved -> Right solved
          Left errors ->
            let
              err = WhileSolving package (NEL.head <$> errors)
              errs = if not continue then mempty else
                case solveAux i continue (package `deleteFrom` r) of
                  Left more -> NEL.toList more
                  Right _ -> mempty
            in Left $ NEL.cons' err errs)
  solvePackage i r package version dependencies =
    let indent = power "  " i in
    let _ = trace [ indent, "TRYING ", PackageName.print package, "@", Version.printVersion version ] unit in
    let required = r.required <> soleVersionOf package version <> dependencies
    in solveAux (i + 1) false (trimReachable { registry: r.registry, required, updated: SemigroupMap $ Map.singleton package $ SemigroupMap $ Map.singleton version $ dependencies })
  deleteFrom package { registry, required } = do
    let
      deleter :: forall v. SemigroupMap PackageName v -> SemigroupMap PackageName v
      deleter = over SemigroupMap (Map.delete package)
    { required: deleter required
    , registry: map deleter <$> deleter registry
    , updated: mempty
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
