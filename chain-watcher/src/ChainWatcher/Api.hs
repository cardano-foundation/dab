
module ChainWatcher.Api where

import ChainWatcher.Types
import Servant
import Servant.API.EventStream

type API =
          Summary "Check health"
       :> "healthcheck"
       :> Get '[JSON] Bool
  :<|>
       "sse"
       :> Capture "client_id" ClientId
       :> ServerSentEvents
  :<|>
       "clients"
       :> ClientsAPI

type ClientsAPI =
           Summary "New client"
        :> Description "Create new client"
        :> "new"
        :> Post '[JSON] ClientId
   :<|>
           Summary "Remove client"
        :> Description "Delete client and all its events"
        :> "remove"
        :> Capture "client_id" ClientId
        :> Post '[JSON] ()
   :<|>
           Summary "Create request"
        :> Description "Create new request for specific client"
        :> "request"
        :> Capture "client_id" ClientId
        :> ReqBody '[JSON] Request
        :> Post '[JSON] Integer
   :<|>
           Summary "Get events"
        :> Description "Query events belonging to specific client"
        :> "events"
        :> Capture "client_id" ClientId
        :> QueryParam "longpoll" Bool
        :> Get '[JSON] [EventDetail]

api :: Proxy API
api = Proxy
