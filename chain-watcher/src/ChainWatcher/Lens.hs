
module ChainWatcher.Lens
  where

import Control.Lens (makeFields, makePrisms)
import ChainWatcher.Types

makePrisms ''Event
makeFields ''EventDetail

makePrisms ''Request
makeFields ''RequestDetail



