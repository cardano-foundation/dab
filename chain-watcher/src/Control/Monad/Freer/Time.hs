-- | Time effect
--

module Control.Monad.Freer.Time
  ( Time (..)
  , getTime
  , runTime
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.TH ( makeEffect )
import Control.Monad.IO.Class (MonadIO (..))

import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX

data Time a where
  GetTime :: Time (POSIXTime)

makeEffect ''Time

runTime
  :: forall a m effs
   . ( LastMember m effs
     , MonadIO m)
  => Eff (Time ': effs) a
  -> Eff effs a
runTime = interpretM $ \case
  GetTime -> liftIO $ Data.Time.Clock.POSIX.getPOSIXTime

