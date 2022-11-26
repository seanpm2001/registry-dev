module Test.Integration
  ( main
  ) where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Effect.Exception as Exception
import Foreign.Git as Git
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.Path as Path
import Node.Process (argv)
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest(..))
import Registry.Scripts.BowerInstaller (BowerSolved)
import Registry.Solver as Solver
import Registry.Version (Range, Version)
import Registry.Version as Version
import Sunde as Process
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = do
  args <- argv
  case Array.drop 2 args of
    [] -> tests
    [package_versionS] | [packageS,versionS] <- String.split (String.Pattern "@") package_versionS -> do
      let
        package = unsafeFromRight $ PackageName.parse packageS
        version = unsafeFromRight $ Version.parseVersion Version.Lenient versionS
      testOne package version
    _ -> logShow args

tests :: Effect Unit
tests = launchAff_ do
  { solverIndex, solutions } <- setup

  runSpec' (defaultConfig { timeout = Nothing }) [ consoleReporter ] do
    Spec.describe "Solves core packages" do
      mkTest solverIndex $ unsafeFromJust $ Map.lookup "purescript" solutions

    Spec.describe "Solves contrib packages" do
      mkTest solverIndex $ unsafeFromJust $ Map.lookup "purescript-contrib" solutions

    Spec.describe "Solves web packages" do
      mkTest solverIndex $ unsafeFromJust $ Map.lookup "purescript-web" solutions

    Spec.describe "Solves node packages" do
      mkTest solverIndex $ unsafeFromJust $ Map.lookup "purescript-node" solutions

type Owner = String

type SegmentedByOwner = Map Owner (Map PackageName (Map Version { bower :: BowerSolved, manifest :: Map PackageName Range }))

segmentSolvableByOwner :: RegistryIndex -> FilePath -> Aff SegmentedByOwner
segmentSolvableByOwner index bowerDir = map snd $ flip State.runStateT Map.empty do
  void $ forWithIndex index \package versions -> do
    let
      readSolutions :: Aff (Either String (Map Version BowerSolved))
      readSolutions = Json.readJsonFile (Path.concat [ bowerDir, PackageName.print package <> ".json" ])
    liftAff readSolutions >>= case _ of
      Left err -> unsafeCrashWith $ "Unable to parse solutions for " <> PackageName.print package <> ": " <> err
      Right solutions -> forWithIndex versions \version (Manifest manifest) -> do
        -- We only include packages that Bower considers solvable.
        case Map.lookup version solutions of
          Nothing -> pure unit
          Just solved -> case manifest.location of
            GitHub { owner } -> do
              let
                value = { bower: solved, manifest: manifest.dependencies }

                modifier :: SegmentedByOwner -> SegmentedByOwner
                modifier = Map.insertWith (Map.unionWith Map.union) owner (Map.singleton package (Map.singleton manifest.version value))

              State.modify_ modifier
            _ ->
              pure unit

testOne :: PackageName -> Version -> Effect Unit
testOne package version = launchAff_ do
  { solverIndex, solutions } <- setup

  runSpec' (defaultConfig { timeout = Nothing }) [ consoleReporter ] do
    Spec.describe "Solves specified package" do
      mkTest solverIndex $ map (select version) $ select package $ Map.unions solutions

select :: forall k15 v16. Ord k15 => k15 -> Map k15 v16 -> Map k15 v16
select k m = case Map.lookup k m of
  Just v -> Map.singleton k v
  Nothing -> Map.empty

setup :: Aff { solverIndex :: Solver.DependencyIndex, solutions :: Map String (Map PackageName (Map Version { bower :: BowerSolved, manifest :: Map PackageName Range })) }
setup = do
  tmp <- liftEffect Tmp.mkTmpDir

  result <- withBackoff' $ Except.runExceptT do
    log "Fetching registry index..."
    Git.runGit_ [ "clone", "https://github.com/purescript/registry-index", "--depth", "500" ] (Just tmp)
    void $ lift $ Process.spawn { cmd: "bash", args: [ "-c", "cd registry-index && git checkout e7346e906d7bb93ed98e7f4c43349ed01b8398ed" ], stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = Just tmp })
    log "Fetching bower solutions..."
    Git.runGit_ [ "clone", "https://github.com/thomashoneyman/bower-solver-results", "--depth", "1" ] (Just tmp)

  case result of
    Nothing -> throwError $ Exception.error "Could not clone registry index and/or solver results."
    Just _ -> pure unit

  log "Reading registry index..."
  index <- Index.readRegistryIndex (Path.concat [ tmp, "registry-index" ])

  -- Note: this segmented index only considers packages that have a
  -- corresponding Bower solution.
  log "Segmenting solvable package versions by owner..."
  segmentedIndex <- segmentSolvableByOwner index $ Path.concat [ tmp, "bower-solver-results" ]

  log "Doing tests …"

  let
    solverIndex :: Solver.DependencyIndex
    solverIndex = map (map (\(Manifest m) -> m.dependencies)) index

  pure { solverIndex, solutions: segmentedIndex }

mkTest :: Solver.DependencyIndex -> Map PackageName (Map Version { bower :: BowerSolved, manifest :: Map PackageName Range }) -> Spec.Spec Unit
mkTest solverIndex pkgs = void $ forWithIndex pkgs \package versions -> do
  Spec.describe ("Solves " <> PackageName.print package) do
    pure unit
    let versionsBackwards = Array.reverse (Map.toUnfoldable versions)
    -- We try comparing Bower's solution from the Bowerfile to our
    -- solver's attempt on the Bowerfile dependencies.
    Spec.it "Bowerfile" do
      pure unit
      for_ versionsBackwards \(Tuple version { bower }) -> do
        solve package version bower.dependencies bower.bowerfileSolution

    -- We also try comparing Bower's solution using the manifest dependencies
    -- to our solver's attempt on the same.
    Spec.it "Manifest file " do
      pure unit
      for_ versionsBackwards \(Tuple version { bower, manifest }) -> do
        solve package version manifest bower.manifestSolution
  where
  solve package version dependencies solution = do
    pure unit
    let
      name = PackageName.print package <> "@" <> Version.printVersion version
      isNoVersionsError = case _ of
        Solver.Conflicts es ->
          es # any \i -> Solver.upperBound i > Solver.lowerBound i
        _ -> false

    case Solver.solve solverIndex dependencies of
      -- If we can't provide a solution because no versions are available in
      -- the index, then we either restricted our ranges too much or the
      -- problem is that our index has fewer versions than Bower's. In the
      -- interest of useful tests, we assume the latter.
      Left errs | any isNoVersionsError errs -> pure unit
      -- Otherwise, we failed to find a solution and failed the test.
      Left errs -> do
        let printedErrs = String.joinWith "\n" $ Array.fromFoldable $ map Solver.printSolverError errs
        Assert.fail $ String.joinWith "\n----------\n"
          [ name
          , printedErrs
          , Json.printJson solution
          ]
      -- If we found a solution then we passed the test.
      -- TODO: We can also check that our solution produces versions as
      -- high as those produced by Bower, if we want.
      Right ourSolution -> do
        forWithIndex_ dependencies \dependency range ->
          case Map.lookup dependency ourSolution of
            Just depVersion ->
              depVersion `Assert.shouldSatisfy` Version.rangeIncludes range
            Nothing -> Assert.fail $ "Missing dependency " <> PackageName.print dependency
