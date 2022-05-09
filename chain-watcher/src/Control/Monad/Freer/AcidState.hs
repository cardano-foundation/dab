
module Control.Monad.Freer.AcidState
  ( evalAcidState
  , openLocalState
  )
  where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Acid
import Data.Acid.Core

evalAcidState
  :: forall s m effs qe fue ue
   . ( QueryEvent qe
     , MethodState qe ~ s
     , MethodResult qe ~ s

     , fue ~ (s -> ue)
     , UpdateEvent ue
     , MethodState ue ~ s
     , MethodResult ue ~ ()

     , MonadIO m
     , LastMember m effs
     )
  => AcidState s
  -> qe
  -> fue
  -> Eff (State s ': effs)
  ~> Eff effs
evalAcidState acid qe ue =
  interpret $ \case
    Get -> liftIO $ query acid qe
    Put x -> liftIO $ update acid (ue x)
