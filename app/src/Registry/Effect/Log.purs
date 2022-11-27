module Registry.Effect.Log where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Effect.Class.Console as Console
import Foreign.GitHub (IssueNumber, Octokit)
import Foreign.GitHub as GitHub
import Node.Process as Process
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

data Log a = Log LogLevel String a

derive instance Functor Log

type LOG r = (log :: Log | r)

_log :: Proxy "log"
_log = Proxy

log :: forall r. LogLevel -> String -> Run (LOG + r) Unit
log level message = Run.lift _log (Log level message unit)

debug :: forall r. String -> Run (LOG + r) Unit
debug = log Debug

info :: forall r. String -> Run (LOG + r) Unit
info = log Info

warn :: forall r. String -> Run (LOG + r) Unit
warn = log Warn

error :: forall r. String -> Run (LOG + r) Unit
error = log Error

die :: forall r a. String -> Run (LOG + AFF + r) a
die message = do
  error message
  Run.liftAff $ liftEffect $ Process.exit 1

-- | Handle the LOG effect in the GitHub environment, logging debug statements
-- | to the console and others to the GitHub issue associated with the execution
handleLogGitHub
  :: forall a r
   . Octokit
  -> IssueNumber
  -> Log a
  -> Run (AFF + r) a
handleLogGitHub octokit issue = case _ of
  Log level message next -> case level of
    Debug -> Run.liftAff do
      Console.debug message
      pure next

    Info -> Run.liftAff do
      Console.info message
      Except.runExceptT (GitHub.createComment octokit issue message) >>= case _ of
        Left err -> unsafeCrashWith (GitHub.printGitHubError err)
        Right _ -> pure next

    Warn -> Run.liftAff do
      Console.warn message
      Except.runExceptT (GitHub.createComment octokit issue message) >>= case _ of
        Left err -> unsafeCrashWith (GitHub.printGitHubError err)
        Right _ -> pure next

    Error -> Run.liftAff do
      Console.error message
      Except.runExceptT (GitHub.createComment octokit issue message) >>= case _ of
        Left err -> unsafeCrashWith (GitHub.printGitHubError err)
        Right _ -> pure next
