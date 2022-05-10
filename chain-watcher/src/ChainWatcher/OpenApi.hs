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
import Servant.OpenApi

import ChainWatcher.Api (api)
import ChainWatcher.Types

instance ToSchema Address
instance ToSchema Slot
instance ToSchema TxHash

instance ToSchema Event
instance ToSchema EventId
instance ToSchema EventDetail
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
