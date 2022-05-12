-- | Based on Servant.Swagger.UI.ReDoc (BSD-3)

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Servant.Swagger.UI.ReDoc.Blaze (
    -- * Swagger UI API
    SwaggerSchemaUI,
    SwaggerSchemaUI',
    redocSchemaUIServer,
    -- ** ReDoc theme
    redocIndexTemplate
    ) where

import Prelude (($), mempty)
import Servant.Swagger.UI.Core

import Data.Text       (Text)
import Servant
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy

-- | Serve alternative Swagger UI.
--
-- See <https://github.com/Rebilly/ReDoc/>
redocSchemaUIServer
    :: (Server api ~ Handler a)
    => Text
    -> a
    -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer title' =
    swaggerSchemaUIServerImpl (redocIndexTemplate title') mempty

redocIndexTemplate :: Text -> Text
redocIndexTemplate title' = Data.Text.Lazy.toStrict $ renderHtml
  $ docTypeHtml $ do
    head $ do
      title (toHtml title')
      meta ! A.charset "utf-8"
      meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    body $ do
      (preEscapedToHtml @Text "<redoc spec-url='../SERVANT_SWAGGER_UI_SCHEMA'></redoc>")
      script mempty ! A.src "https://cdn.jsdelivr.net/npm/redoc@next/bundles/redoc.standalone.js"
