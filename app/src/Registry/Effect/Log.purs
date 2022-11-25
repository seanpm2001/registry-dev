module Registry.Effect.Log where

import Registry.App.Prelude hiding (log)

import Run (Run)
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
