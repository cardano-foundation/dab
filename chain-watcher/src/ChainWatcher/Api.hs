
module ChainWatcher.Api where

import ChainWatcher.Types
import Servant
import Servant.API.EventStream

type API =
       "healthcheck" :> Get '[JSON] Bool
  :<|> "sse" :> Capture "client_id" ClientId :> ServerSentEvents
  :<|> "clients" :> ClientsAPI
  :<|> "demo" :> Raw

type ClientsAPI =
        "new"
    :> Post '[JSON] ClientId
   :<|>
        "remove"
    :> Capture "client_id" ClientId
    :> Post '[JSON] ()
   :<|>
        "request"
    :> Capture "client_id" ClientId
    :> ReqBody '[JSON] Request
    :> Post '[JSON] Integer
   :<|>
        "events"
    :> Capture "client_id" ClientId
    :> Get '[JSON] [EventDetail]

api :: Proxy API
api = Proxy
