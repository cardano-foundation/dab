-- | Log effect
-- mimics co-log-polysemy
-- https://github.com/kowainik/co-log/blob/main/co-log-polysemy/src/Colog/Polysemy/Effect.hs

module Control.Monad.Freer.Log
  ( Log (..)
  , logs
  , mapLog
  , runLogAction
  , runLogPrettySimple
  , runLogWriter
  , runNoLog
  ) where

import Colog.Core.Action (LogAction (..))
import Colog.Core.IO (logStringStdout)
import Control.Monad.Freer
import Control.Monad.Freer.Writer
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Kind (Type)
import qualified Data.Text
import qualified Data.Text.Lazy
import Text.Pretty.Simple

data Log (logType :: Type) a where
  Logs :: logType -> Log logType ()

-- | Log something
logs :: forall (logType :: Type) effs
  . Member (Log logType) effs
  => logType
  -> Eff effs ()
logs = send . Logs

-- | Run @Log logType@ using @LogAction IO logType@ from co-log.
runLogAction
  :: forall logType effs a
  . (LastMember IO effs)
  => LogAction IO logType
  -> Eff (Log logType ': effs) a
  -> Eff effs a
runLogAction (LogAction action) = interpret $ \case
  Logs msg -> liftIO $ action msg

-- | Silent
runNoLog
  :: forall logType effs a
   .  Eff (Log logType ': effs) a
  -> Eff effs a
runNoLog = interpret $ \case
  Logs _ -> pure ()

-- | Log to @Writer [logType]@
runLogWriter
  :: forall logType effs a
   .  Eff (Log logType ': effs) a
  -> Eff effs (a, [logType])
runLogWriter  = runWriter . reinterpret go
  where go :: Log logType v -> Eff (Writer [logType] ': effs) v
        go = \case
          Logs a -> tell [a]

-- | Log to stdout using pretty-simple
runLogPrettySimple
  :: forall logType effs a
   . ( LastMember IO effs
     , Show logType)
  => Eff (Log logType ': effs) a
  -> Eff effs a
runLogPrettySimple =
  runLogAction @logType (contramap (Data.Text.unpack  . Data.Text.Lazy.toStrict . pShow) logStringStdout)

-- | Map from one Log type to another
mapLog
  :: forall logType toType effs a
   .  (logType -> toType)
  -> Eff (Log logType ': effs) a
  -> Eff (Log toType ': effs) a
mapLog f = reinterpret $ \case
  Logs x -> logs (f x)
