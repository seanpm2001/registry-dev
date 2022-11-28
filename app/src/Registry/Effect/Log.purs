module Registry.Effect.Log where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Formatter.DateTime as Formatter.DateTime
import Effect.Class.Console as Console
import Foreign.GitHub (IssueNumber, Octokit)
import Foreign.GitHub as GitHub
import Node.FS.Aff as FS.Aff
import Node.Process as Process
import Registry.Constants as Constants
import Registry.Internal.Format as Internal.Format
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

data Log a = Log LogLevel String a

derive instance Functor Log

-- | An effect for reporting events as messages
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
-- | to the console only and others to both the console and the GitHub issue
-- | associated with the execution. Suitable for use in GitHub events.
handleLogGitHub :: forall a r. Octokit -> IssueNumber -> Log a -> Run (AFF + r) a
handleLogGitHub octokit issue = case _ of
  Log level message next -> case level of
    Debug -> Run.liftAff do
      Console.debug message
      pure next

    Info -> Run.liftAff do
      Console.info message
      attemptComment message
      pure next

    Warn -> Run.liftAff do
      Console.warn message
      attemptComment message
      pure next

    Error -> Run.liftAff do
      Console.error message
      attemptComment message
      pure next
  where
  attemptComment message =
    GitHub.request octokit (GitHub.createComment Constants.registry issue message) >>= case _ of
      Left err -> do
        Console.error "UNEXPECTED ERROR: Could not send comment to GitHub."
        Console.error $ GitHub.printGitHubError err
        liftEffect $ Process.exit 1
      Right _ -> pure unit

-- | Write logs to the console and to the given logfile. Suitable for use in
-- | local runs of the registry tools.
handleLogFile :: forall a r. FilePath -> Log a -> Run (AFF + r) a
handleLogFile logfile = case _ of
  Log level message next -> Run.liftAff case level of
    Debug -> do
      Console.debug message
      writeTimestamped message
      pure next

    Info -> do
      Console.info message
      writeTimestamped message
      pure next

    Warn -> do
      Console.warn message
      writeTimestamped message
      pure next

    Error -> do
      Console.error message
      writeTimestamped message
      pure next
  where
  writeTimestamped message = do
    now <- liftEffect nowUTC
    let formatted = Array.fold [ "[", Formatter.DateTime.format Internal.Format.iso8601DateTime now, "] ", message, "\n" ]
    FS.Aff.appendTextFile UTF8 logfile formatted
