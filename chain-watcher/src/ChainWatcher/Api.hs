
module ChainWatcher.Api where

import Servant
import Servant.API.EventStream
import ChainWatcher.Types

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

api :: Proxy API
api = Proxy
