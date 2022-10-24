module Test.Registry.SolverTest where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap, sum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List ((:))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Newtype (unwrap, wrap)
import Data.String.CodeUnits as String
import Effect.Console (time, timeEnd)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Git as Git
import Node.FS.Aff (appendTextFile, writeTextFile)
import Node.Path as Path
import Registry.Index as Index
import Registry.Json (encode, printJson, writeJsonFile)
import Registry.PackageName as PackageName
import Registry.Solver (TransitivizedRegistry)
import Registry.Solver as Solver
import Registry.Version (parseVersion, printVersion)
import Registry.Version as Version
import Safe.Coerce (coerce)

logJson :: forall m a. MonadEffect m => RegistryJson a => a -> m Unit
logJson = log <<< printJson <<< encode

perf :: forall b. String -> (Unit -> b) -> b
perf name f = unsafePerformEffect do
  time name
  let b = f unit
  timeEnd name
  pure b

guarded :: forall a. Eq a => Monoid a => a -> Maybe a
guarded a = if a == mempty then Nothing else Just a

printJsonI :: forall a. RegistryJson a => a -> String
printJsonI = Array.singleton >>> printJson >>> String.drop 2 >>> String.dropRight 2

main :: Effect Unit
main = launchAff_ do
  void $ Except.runExceptT $ Git.runGit_ [ "clone", "https://github.com/purescript/registry-index", "--depth", "1" ] (Just "tmp")
  index <- Index.readRegistryIndex (Path.concat [ "tmp", "registry-index" ])
  writeJsonFile "./index.json" index
  let
    registry = map (unwrap >>> _.dependencies) <$> index
    ex = unsafeFromJust (Map.lookup (unsafeFromRight (parseVersion Version.Lenient "7.1.0")) (unsafeFromJust (Map.lookup (unsafeFromRight (PackageName.parse "arrays")) registry)))
  writeJsonFile "deps.json" registry
  logShow $ sum $ map Map.size registry
  logJson ex
  logJson $ lmap (Array.fromFoldable) $ perf "solve ex" \_ -> Solver.solve registry ex
  let
    good i = Solver.lowerBound i < Solver.upperBound i
    downcast i | good i = Right $ ">=" <> printVersion (Solver.lowerBound i) <> " <" <> printVersion (Solver.upperBound i)
    downcast i = Left $ "<" <> printVersion (Solver.upperBound i) <> " >=" <> printVersion (Solver.lowerBound i)
    r0 = mapWithIndex (\package -> mapWithIndex \version -> map (Solver.intersectionFromRange package version)) $ coerce registry
    t0 = Tuple (wrap (map Map.keys registry)) r0
    t1 = perf "transitivize1" \_ -> Solver.exploreTransitiveDependencies t0
    t@(Tuple _ reg) = perf "transitivizeAll" \_ -> Solver.fixEq Solver.exploreTransitiveDependencies t1
    numberedList = foldMapWithIndex \i x -> show (i + 1) <> ". " <> x <> "\n"
    unsolvables :: String
    unsolvables = numberedList $ reg #
      foldMapWithIndex \package ->
        Array.reverse <<< foldMapWithIndex \version required ->
          let
            es = either (Array.singleton <<< Solver.Conflicts) mempty $ Solver.checkRequired { registry: reg, required }
          in if Array.null es then mempty else Array.singleton $ Array.fold
            [ PackageName.print package <> "@" <> printVersion version
            , es # foldMap \e -> "\n  " <> Solver.printErrorAt "  " e
            ]
    not_solved :: Tuple String String
    not_solved = bimap numberedList numberedList $ reg #
      foldMapWithIndex \package ->
        bimap Array.reverse Array.reverse <<< foldMapWithIndex \version required ->
          case Solver.checkSolved { registry: reg, required } of
            Left _ -> flip Tuple [] $ Array.singleton $ Array.fold
              [ PackageName.print package <> "@" <> printVersion version
              ]
            Right sol -> Tuple [] $ Array.singleton $ Array.fold
              [ PackageName.print package <> "@" <> printVersion version
              , "\n"
              , printJsonI sol
              ]
    diff :: TransitivizedRegistry -> TransitivizedRegistry -> _
    diff x x' =
      let x0 = (unwrap <<< map (unwrap <<< map unwrap)) x in
      x' # unwrap # Map.mapMaybeWithKey \package versions ->
        guarded $ versions # unwrap #
            Map.mapMaybeWithKey \version required -> do
              let
                gather a b = guarded $ required # unwrap # Map.mapMaybeWithKey \dep range ->
                  let range' = Map.lookup package x0 >>= Map.lookup version >>= Map.lookup dep in
                  if map downcast range' == Just (downcast range) || Tuple a b /= Tuple (map good range') (good range) then Nothing else
                    Just $ map (either identity identity <<< downcast) <$> [range', Just range]
                gathered =
                  { failed: gather (Just true) false <> gather Nothing false
                  , found: map (_ Array.!! 1) <$> (gather Nothing true)
                  , tightened: gather (Just true) true
                  }
              guarded gathered
    loop x i = do
      let
        x' = perf ("transitivize@" <> show (i + 1)) \_ ->
          Solver.exploreTransitiveDependencies x
        _ignored = if i < 5 then Just $ perf ("ex@" <> show (i + 1)) \_ ->
          Solver.solve' (snd x') ex else Nothing
      if x == x'
        then pure unit
        else do
          let r = { diff: diff (snd x) (snd x'), seen: unwrap $ map Array.fromFoldable $ fst x' }
          appendTextFile UTF8 "transitiveSteps.json" $
            printJson r <> "\n"
          loop x' (i + 1)
  writeJsonFile "transitive1.json" $ map map map (partitionMap downcast) $ map map map unwrap $ map unwrap $ unwrap $ snd $ t1
  writeJsonFile "transitive.json" $ map map map (partitionMap downcast) $ map map map unwrap $ map unwrap $ unwrap $ snd $ t
  writeJsonFile "transitiveD.json" $ diff (snd t1) (snd t)
  let
    _ture = perf "compare" \_ -> t == t
    _ex' = perf "solve ex'" \_ -> Solver.solve' reg ex
  writeTextFile UTF8 "unsolvables.txt" unsolvables
  writeTextFile UTF8 "notsolved.txt" (fst not_solved)
  writeTextFile UTF8 "solved.txt" (snd not_solved)
  writeTextFile UTF8 "transitiveSteps.json" ""
  loop t0 0
