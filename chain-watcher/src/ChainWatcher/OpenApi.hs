{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChainWatcher.OpenApi where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Yaml (encodeFile)
import Servant.API.EventStream
import Servant.Docs (ToSample (toSamples), singleSample)
import Servant.OpenApi

import ChainWatcher.Api (api)
import ChainWatcher.Types
import ChainWatcher.OpenApi.Util

import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString)

deriving newtype instance ToSchema Address
deriving newtype instance ToSchema Slot
deriving newtype instance ToSchema TxHash

instance ToSchema Event
instance ToSchema EventId

sampleClientId :: ClientId
sampleClientId = ClientId $ fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

deriving newtype instance ToParamSchema ClientId
deriving newtype instance ToSchema ClientId

sampleUUID :: UUID
sampleUUID = fromJust $ fromString "b2cc10e1-57d6-4b6f-9899-38d972112d8c"

instance ToSample EventDetail where
  toSamples = pure $ singleSample
    EventDetail
      { eventDetailRequestId = 0
      , eventDetailEventId   = EventId 0 sampleUUID
      , eventDetailClientId  = sampleClientId
      , eventDetailTime      = 1612543814
      , eventDetailEvent     = AddressFundsChanged "addrClientA"
      , eventDetailBlock     = 1
      , eventDetailAbsSlot   = 1
      }

instance ToSchema EventDetail where
  declareNamedSchema = genericDeclareNamedSchemaPrefix "eventDetail"

instance ToSchema Request

instance HasOpenApi (ServerSentEvents) where
  toOpenApi _ = mempty

-- | Swagger spec for ChainWatcher APIs.
chainwatcherSwaggerFor :: (HasOpenApi api) => Proxy api -> OpenApi
chainwatcherSwaggerFor p = toOpenApi p
  & info.title   .~ "ChainWatcher API"
  & info.version .~ "1.0"
  & info.description ?~ "DAB ChainWatcher API"
  & info.license ?~ ("ASL2" & url ?~ URL "http://www.apache.org/licenses")

addServers :: [Server] -> OpenApi -> OpenApi
addServers s o =
  o & servers .~ s

makeServer :: Text -> Text -> Server
makeServer url' desc = Server url' (Just desc) mempty

-- | Swagger spec for ChainWatcher API.
chainwatcherSwagger :: OpenApi
chainwatcherSwagger = chainwatcherSwaggerFor api

-- | Output generated @swagger.json@ file
writeSwaggerJSON :: IO ()
writeSwaggerJSON =
  Data.ByteString.Lazy.Char8.writeFile
    "swagger.json"
    $ encodePretty chainwatcherSwagger

-- | Output generated @swagger.json@ file
writeSwaggerYAML :: IO ()
writeSwaggerYAML =
  encodeFile
    "swagger.yaml"
    chainwatcherSwagger
