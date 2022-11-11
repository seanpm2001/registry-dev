module Test.Registry.SolveAll where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String (stripPrefix)
import Data.String as String
import Effect.Console (time, timeEnd)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Git as Git
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (argv)
import Registry.Index as Index
import Registry.Json (encode, printJson)
import Registry.PackageName as PackageName
import Registry.Solver as Solver
import Registry.Version as Version

packages :: Array PackageName.PackageName
packages = Array.mapMaybe (hush <<< PackageName.parse)
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "catenable-lists"
  , "console"
  , "const"
  , "contravariant"
  , "control"
  , "datetime"
  , "distributive"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "exists"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "free"
  , "functions"
  , "functors"
  , "gen"
  , "graphs"
  , "identity"
  , "integers"
  , "invariant"
  , "lazy"
  , "lcg"
  , "lists"
  , "maybe"
  , "minibench"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "orders"
  , "parallel"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "record"
  , "refs"
  , "safe-coerce"
  , "semirings"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "validation"
  , "ace"
  , "aff"
  , "aff-bus"
  , "aff-coroutines"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "argonaut-traversals"
  , "arraybuffer"
  , "arraybuffer-types"
  , "avar"
  , "bigints"
  , "colors"
  , "concurrent-queues"
  , "coroutines"
  , "css"
  , "fixed-points"
  , "float32"
  , "fork"
  , "form-urlencoded"
  , "formatters"
  , "freet"
  , "github-actions-toolkit"
  , "http-methods"
  , "int64"
  , "js-date"
  , "js-promise"
  , "js-timers"
  , "js-uri"
  , "machines"
  , "matryoshka"
  , "media-types"
  , "nullable"
  , "options"
  , "parsing"
  , "pathy"
  , "precise"
  , "profunctor-lenses"
  , "quickcheck-laws"
  , "rationals"
  , "react"
  , "react-dom"
  , "routing"
  , "string-parsers"
  , "strings-extra"
  , "these"
  , "uint"
  , "unicode"
  , "unsafe-reference"
  , "uri"
  , "canvas"
  , "web-clipboard"
  , "web-cssom"
  , "web-dom"
  , "web-dom-parser"
  , "web-dom-xpath"
  , "web-encoding"
  , "web-events"
  , "web-fetch"
  , "web-file"
  , "web-html"
  , "web-pointerevents"
  , "web-promise"
  , "web-socket"
  , "web-storage"
  , "web-streams"
  , "web-touchevents"
  , "web-uievents"
  , "web-xhr"
  , "node-buffer"
  , "node-buffer-blob"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-net"
  , "node-path"
  , "node-process"
  , "node-readline"
  , "node-streams"
  , "node-url"
  , "posix-types"
  ]

logJson :: forall m a. MonadEffect m => RegistryJson a => a -> m Unit
logJson = log <<< printJson <<< encode

perf :: forall b. String -> (Unit -> b) -> b
perf name f = unsafePerformEffect do
  time name
  let b = f unit
  timeEnd name
  pure b

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect argv
  void $ Except.runExceptT $ Git.runGit_ [ "clone", "https://github.com/purescript/registry-index", "--depth", "1" ] (Just "tmp")
  index <- Index.readRegistryIndex (Path.concat [ "tmp", "registry-index" ])
  let registry = map (unwrap >>> _.dependencies) <$> index
  case Array.drop 2 args of
    ["--all"] -> do
      forWithIndex_ registry \package versions -> do
        forWithIndex_ versions \version deps -> do
          test registry package version deps
    ["--notsolved"] -> do
      let
        notsolvedP :: String -> Array (Tuple PackageName.PackageName Version.Version)
        notsolvedP = String.split (String.Pattern "\n") >>> Array.dropEnd 1 >>> map \line ->
          case String.split (String.Pattern ". ") line of
            [_, package_version] ->
              case String.split (String.Pattern "@") package_version of
                [package, version] ->
                  case Tuple
                    <$> PackageName.parse package
                    <*> Version.parseVersion Version.Strict version
                    of
                      Left e -> unsafeCrashWith (show { e, package, version })
                      Right v -> v
                _ -> unsafeCrashWith "bad @"
            _ -> unsafeCrashWith $ "bad ."
      notsolvedTxt <- readTextFile UTF8 "notsolved.txt"
      let notsolveds = notsolvedTxt # notsolvedP
      for_ notsolveds \(Tuple package version) -> do
        when (isNothing $ stripPrefix (String.Pattern "aws-") (show package)) do
          let versions = unsafeFromJust $ Map.lookup package registry
          let deps = unsafeFromJust $ Map.lookup version versions
          test registry package version deps
    [package_versionS] | [packageS,versionS] <- String.split (String.Pattern "@") package_versionS -> do
      let
        package = unsafeFromRight $ PackageName.parse packageS
        version = unsafeFromRight $ Version.parseVersion Version.Lenient versionS
      let versions = unsafeFromJust $ Map.lookup package registry
      let deps = unsafeFromJust $ Map.lookup version versions
      test registry package version deps
    [] -> do
      for_ packages \package -> do
        let versions = unsafeFromJust $ Map.lookup package registry
        forWithIndex_ versions \version deps -> do
          test registry package version deps
    _ -> do
      log "Either use --all, --notsolved, or no args"
  where
  test registry package version deps = do
    log $ "%%% Solving " <> show package <> "@" <> show version <> " %%%"
    case Solver.solve registry deps of
      Right _ -> pure unit
      Left es -> do
        log $ "Failed: " <> show package <> "@" <> show version
        log $ foldMap Solver.printSolverError es
