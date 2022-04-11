{-# LANGUAGE OverloadedStrings #-}

module ClientStateSpec where

import ChainWatcher.Types
import Data.Maybe
import qualified Data.Set
import Data.Text (Text)
import Data.UUID (UUID, fromString)
import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

-- temp
import Blockfrost.Freer.Client (Address, Slot (..), TxHash)

sampleClientAId :: UUID
sampleClientAId = fromMaybe (error "absurd") $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

reqA = RequestDetail {
    requestDetailRequestId = 0
  , requestDetailClientId = sampleClientAId
  , requestDetailRequest = AddressFundsRequest "addrClientA"
  , requestDetailTime = 1612543814
  }

recurringReqA = reqA { requestDetailRequest = Recurring (requestDetailRequest reqA) }

sampleClientA = newClientState
  { clientStateRequests = Data.Set.fromList [reqA]
  , clientStateLastId = 1
  }

evtA = EventDetail {
    eventDetailEventId = 0
  , eventDetailClientId = sampleClientAId
  , eventDetailEvent = AddressFundsChanged "addrClientA"
  , eventDetailTime = 1612543814
  , eventDetailAbsSlot = 1
  , eventDetailBlock = 1
  }

rollbackEvtA :: EventDetail
rollbackEvtA = evtA { eventDetailEvent = Rollback (eventDetailEvent evtA)  }

eventWithNoRequest :: EventDetail
eventWithNoRequest =
   EventDetail {
    eventDetailEventId = -1
  , eventDetailClientId = sampleClientAId
  , eventDetailEvent = UtxoProduced "noSuchAddr" ["noSuchTxHash"]
  , eventDetailTime = 1622543814
  , eventDetailAbsSlot = 1
  , eventDetailBlock = 1
  }

spec_client_state :: Spec
spec_client_state = do
  it "updates single client on event" $ do
    updateClientState evtA sampleClientA
    `shouldBe`
    Just (sampleClientA
      { clientStateRequests = mempty
      , clientStatePendingEvents = [evtA]
      })

  it "single event with rollback event" $ do
    s1 <- updateClientState evtA sampleClientA
    updateClientState rollbackEvtA s1
    `shouldBe`
    Just sampleClientA

  it "rollback after client takes pending events" $ do
    s1 <- updateClientState evtA sampleClientA
    let (evts, s2) = takeEvents s1
    let s3 = updateClientState rollbackEvtA s2
    s3
    `shouldBe`
    Just (sampleClientA
      { clientStatePendingEvents = [rollbackEvtA]
      , clientStatePastEvents = [evtA]
      })

  it "no update on unrelated event" $ do
    updateClientState eventWithNoRequest sampleClientA
    `shouldBe`
    Nothing


  it "preserves recurring requests" $ do
    updateClientState evtA
      (sampleClientA
        { clientStateRequests = Data.Set.fromList [reqA, recurringReqA]})
    `shouldBe`
    Just (sampleClientA
      { clientStateRequests = Data.Set.fromList [recurringReqA]
      , clientStatePendingEvents = [evtA]
      })


-- generic arbitrary, but not sure if this is helpful at all
-- since we need well formed ordering of requests / cancel request
-- or events / rollback (events)
instance Arbitrary Request where
  -- arbitrary = -- oneof (map pure [ SlotRequest 2 ])
  arbitrary =  oneof ([ SlotRequest . Slot <$> arbitrary ])
