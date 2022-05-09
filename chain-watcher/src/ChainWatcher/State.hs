-- Due to SafeCopy Money.Discrete' instance we need
{-# LANGUAGE UndecidableInstances #-}
-- otherwise we get
-- Illegal nested constraint ‘GHC.TypeNats.KnownNat
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChainWatcher.State
  ( QueryState(..)
  , WriteState(..)
  ) where

import Data.Acid
import Data.SafeCopy
import Data.Typeable

import qualified Control.Monad.Reader
import qualified Control.Monad.State

import Blockfrost.Freer.Client hiding (api)
import Money
import GHC.TypeLits (KnownSymbol)

$(deriveSafeCopy 0 'base ''Scale)
$(deriveSafeCopy 0 'base ''SomeDiscrete)

-- | Compatible with 'Money.SomeDiscrete'.
instance
  ( KnownSymbol currency, Typeable scale, Money.GoodScale scale
  ) => SafeCopy (Money.Discrete' currency scale) where
  putCopy = contain . safePut . Money.toSomeDiscrete
  getCopy = contain $ maybe (fail "Discrete'") pure
             =<< fmap Money.fromSomeDiscrete safeGet

$(deriveSafeCopy 0 'base ''BlockHash)
$(deriveSafeCopy 0 'base ''Slot)
$(deriveSafeCopy 0 'base ''Epoch)
$(deriveSafeCopy 0 'base ''Block)

writeState :: [Block] -> Update [Block] ()
writeState = Control.Monad.State.put

queryState :: Query [Block] [Block]
queryState =  Control.Monad.Reader.ask

type Blocks = [Block]

$(makeAcidic ''Blocks ['writeState, 'queryState])
