module Registry.App.CLI.Git where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.String as String
import Effect.Aff as Aff
import Node.ChildProcess as NodeProcess
import Sunde as Process

runGit_ :: Array String -> Maybe FilePath -> ExceptT String Aff Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right stdout
    _ -> Left stderr

runGitSilent :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGitSilent args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  case result.exit of
    NodeProcess.Normally 0 -> do
      let stdout = String.trim result.stdout
      pure $ Right stdout
    _ -> pure $ Left $ "Failed to run git command via runGitSilent."

-- | Clone a package from a Git location to the provided directory.
cloneGitTag :: String -> String -> FilePath -> Aff Unit
cloneGitTag url ref targetDir = do
  let args = [ "clone", url, "--branch", ref, "--single-branch", "-c", "advice.detachedHead=false" ]
  withBackoff' (Except.runExceptT (runGit args (Just targetDir))) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error $ "Timed out attempting to clone git tag: " <> url <> " " <> ref
    Just (Left err) -> Aff.throwError $ Aff.error err
    Just (Right _) -> pure unit

-- | Read the published time of the checked-out commit.
gitGetRefTime :: String -> FilePath -> ExceptT String Aff DateTime
gitGetRefTime ref repoDir = do
  timestamp <- runGit [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir)
  jsDate <- liftEffect $ JSDate.parse timestamp
  dateTime <- Except.except $ note "Failed to convert JSDate to DateTime" $ JSDate.toDateTime jsDate
  pure dateTime
