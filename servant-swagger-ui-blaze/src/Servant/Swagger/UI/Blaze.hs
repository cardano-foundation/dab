-- | Based on Servant.Swagger.UI (BSD-3)

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

module Servant.Swagger.UI.Blaze (
    -- * Swagger UI API
    SwaggerSchemaUI,
    SwaggerSchemaUI',
    swaggerSchemaUIServer,
    swaggerIndexTemplate
    ) where

import Prelude (($), (<>), mempty)
import Servant.Swagger.UI.Core

import Data.Text       (Text)
import Servant
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text
import qualified Data.Text.Lazy

-- | Serve alternative Swagger UI.
--
-- See <https://github.com/swagger-api/swagger-ui/>
swaggerSchemaUIServer
    :: (Server api ~ Handler a)
    => Text
    -> a
    -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer title' =
    swaggerSchemaUIServerImpl (swaggerIndexTemplate title') mempty

swaggerIndexTemplate :: Text -> Text
swaggerIndexTemplate title' = Data.Text.Lazy.toStrict $ renderHtml
  $ docTypeHtml $ do
    head $ do
      title (toHtml title')
      link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (fromUnpkg "swagger-ui.css")
      meta ! A.charset "utf-8"
      meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      style (preEscapedToHtml @Text leStyle)
    body $ do
      div mempty ! A.id "swagger-ui"
      (preEscapedToHtml @Text leScript)
      script mempty ! A.src (fromUnpkg "swagger-ui-bundle.js")
      script mempty ! A.src (fromUnpkg "swagger-ui-standalone-preset.js")
  where
    fromUnpkg f = "https://unpkg.com/swagger-ui-dist/" <> f

leScript :: Text
leScript = Data.Text.unlines [
    "<script>"
  , "window.onload = function() {"
  , "  const ui = SwaggerUIBundle({ "
  , "    url: '../SERVANT_SWAGGER_UI_SCHEMA', "
  , "    dom_id: '#swagger-ui', "
  , "    deepLinking: true, "
  , "    presets: [ "
  , "      SwaggerUIBundle.presets.apis, "
  , "      SwaggerUIStandalonePreset "
  , "    ], "
  , "    plugins: [ "
  , "      SwaggerUIBundle.plugins.DownloadUrl "
  , "    ], "
  , "    layout: 'StandaloneLayout' "
  , "  }); "
  , "  window.ui = ui;"
  , "};</script>"
  ]

leStyle :: Text
leStyle =  Data.Text.unlines [
    " html"
  , " {"
  , "   box-sizing: border-box;"
  , "   overflow: -moz-scrollbars-vertical;"
  , "   overflow-y: scroll;"
  , " }"
  , " *,"
  , " *:before,"
  , " *:after"
  , " {"
  , "   box-sizing: inherit;"
  , " }"
  , " body"
  , " {"
  , "   margin:0;"
  , "   background: #fafafa;"
  , " }"
  ]
