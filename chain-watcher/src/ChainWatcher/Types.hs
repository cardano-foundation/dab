
module ChainWatcher.Types where

import Control.Lens (makeFields, makePrisms)
import Control.Monad.Freer.TH (makeEffect)
import Data.Map (Map)
import qualified Data.Map
import Data.Set (Set)
import qualified Data.Set
import Deriving.Aeson

import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID (UUID)

import Blockfrost.Freer.Client (Address, Slot, TxHash)

type Confirmations = Integer
type Tx = TxHash
type TxOutRef = (Tx, Integer)

data Event =
    Pong Slot
  | SlotReached Slot
  | UtxoSpent TxOutRef Tx
  | UtxoProduced Address [Tx]
  | TransactionConfirmed Tx
  | TransactionTentative Tx Confirmations
  | AddressFundsChanged Address
  | Rollback Event
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

makePrisms ''Event

type ClientId = UUID

data EventDetail = EventDetail {
    eventDetailEventId  :: Integer
  , eventDetailClientId :: ClientId
  , eventDetailTime     :: POSIXTime
  , eventDetailEvent    :: Event
  , eventDetailBlock    :: Integer
  , eventDetailAbsSlot  :: Slot
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "eventDetail", CamelToSnake]] EventDetail

makeFields ''EventDetail

data Request =
    Ping
  | SlotRequest Slot
  | UtxoSpentRequest TxOutRef
  | UtxoProducedRequest Address
  | TransactionStatusRequest Tx
  | AddressFundsRequest Address
  | Cancel Request -- ^ Cancel previous request
  | Recurring Request -- ^ Recurring request is not removed automatically
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- possibly
-- TxOut status change

makePrisms ''Request

unRecurring :: Request -> Request
unRecurring (Recurring r) = r
unRecurring r             = r

data RequestDetail = RequestDetail {
    requestDetailRequestId :: Integer
  , requestDetailClientId  :: ClientId
  , requestDetailRequest   :: Request
  , requestDetailTime      :: POSIXTime
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

makeFields ''RequestDetail

eventToRequest :: Event -> Request
eventToRequest (Pong _s)                  = Ping
eventToRequest (SlotReached s)            = SlotRequest s
eventToRequest (UtxoSpent txoref _)       = UtxoSpentRequest txoref
eventToRequest (UtxoProduced addr _)      = UtxoProducedRequest addr
eventToRequest (TransactionConfirmed t)   = TransactionStatusRequest t
eventToRequest (TransactionTentative t _) = TransactionStatusRequest t
eventToRequest (AddressFundsChanged addr) = AddressFundsRequest addr
eventToRequest (Rollback e)               = eventToRequest e

eventDetailToRequestDetail :: EventDetail -> RequestDetail
eventDetailToRequestDetail ed = RequestDetail {
    requestDetailRequestId = eventDetailEventId ed
  , requestDetailClientId = eventDetailClientId ed
  , requestDetailRequest = eventToRequest $ eventDetailEvent ed
  , requestDetailTime = eventDetailTime ed }

data WatchSource a where
  GetRequests :: WatchSource (Set RequestDetail)
  ProduceEvent  :: EventDetail -> WatchSource ()

makeEffect ''WatchSource

data TransportType =
    SSE
  | Webhook
  | SlashEvents
  deriving (Eq, Show)

data ClientState = ClientState {
    clientStateRequests      :: Set RequestDetail
  , clientStatePendingEvents :: [EventDetail]
  , clientStatePastEvents    :: [EventDetail]
  , clientStateLastId        :: Integer
  } deriving (Eq, Show)

maxPastEvents :: Integer
maxPastEvents = 100
-- holding past events may allow client to resume from older index, but why
-- for /events api or delayed mode
-- so we can mutate pending

newClientState :: ClientState
newClientState = ClientState {
    clientStateRequests = mempty
  , clientStatePendingEvents = mempty
  , clientStatePastEvents = mempty
  , clientStateLastId = 0
  }

-- Process new event, updating client state if it matches its requests
updateClientState :: EventDetail -> ClientState -> Maybe ClientState
updateClientState evt cs =
  case eventDetailEvent evt of
    -- In case of rollback event which was already propagated
    -- we add rollback event to pending and restore subscription
    Rollback _e | hasPastEvent evt cs ->
      Just $ cs
        { clientStatePendingEvents = clientStatePendingEvents cs ++ [evt]
        , clientStateRequests = Data.Set.insert (eventDetailToRequestDetail evt) (clientStateRequests cs)
        }

    -- In case of rollback event which is still pending reception by client
    -- we drop it from pending requests and restore subscription
    Rollback _e | hasPendingEvent evt cs ->
      Just $ cs
        { clientStatePendingEvents = filter (\ed -> eventDetailEventId ed /= eventDetailEventId evt) (clientStatePendingEvents cs)
        , clientStateRequests = Data.Set.insert (eventDetailToRequestDetail evt) (clientStateRequests cs)
        }

    _ | hasRequest (eventDetailEventId evt) cs ->
      Just $ cs
        { clientStatePendingEvents = clientStatePendingEvents cs ++ [evt]
        , clientStateRequests =
            Data.Set.filter
              (\rd ->
                 requestDetailRequestId rd /= eventDetailEventId evt
              || recurring rd
              )
              (clientStateRequests cs)
        }
    _ | otherwise -> Nothing

recurring :: RequestDetail -> Bool
recurring RequestDetail { requestDetailRequest = Recurring _ } = True
recurring _                                                    = False

hasRequest :: Integer -> ClientState -> Bool
hasRequest rid =
  not
  . Data.Set.null
  . Data.Set.filter
      (\rd -> requestDetailRequestId rd == rid)
  . clientStateRequests

hasPastEvent :: EventDetail -> ClientState -> Bool
hasPastEvent eid =
    any (\ed -> eventDetailEventId ed == eventDetailEventId eid)
  . clientStatePastEvents

hasPendingEvent :: EventDetail -> ClientState -> Bool
hasPendingEvent eid =
    any (\ed -> eventDetailEventId ed == eventDetailEventId eid)
  . clientStatePendingEvents

takeEvents :: ClientState -> ([EventDetail], ClientState)
takeEvents cs =
  let pending = clientStatePendingEvents cs
  in (pending, cs { clientStatePendingEvents = mempty
                  , clientStatePastEvents = clientStatePastEvents cs ++ pending})

routeEvent :: EventDetail -> Map ClientId ClientState -> Map ClientId ClientState
routeEvent evt clients =
  case Data.Map.lookup (eventDetailClientId evt) clients of
    Nothing -> clients
    Just cs -> case updateClientState evt cs of
      Nothing -> clients
      Just newCs -> Data.Map.adjust (pure newCs) (eventDetailClientId evt) clients
