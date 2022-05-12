module ChainWatcher.OpenApi.Util where

import Control.Lens (mapped, (&), (?~))
import Data.Aeson (Options (fieldLabelModifier, constructorTagModifier), camelTo2, defaultOptions, ToJSON (..), Value)
import Data.OpenApi
  ( Definitions
  , NamedSchema (..)
  , Schema
  , SchemaOptions (unwrapUnaryRecords)
  , ToSchema (..)
  , declareSchema
  , example
  , fromAesonOptions
  , genericDeclareNamedSchema
  , schema
  )
import Data.Char (toLower)
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic, Rep)
import Servant.Docs (ToSample (toSamples))

aesonOptions :: Maybe String -> Options
aesonOptions mPrefix = defaultOptions {
   fieldLabelModifier = camelTo2 '_'  . dropIfPrefixed
 , constructorTagModifier = map toLower
 }
 where dropIfPrefixed = maybe id (drop . length) mPrefix

schemaOptions :: Maybe String -> SchemaOptions
schemaOptions p =
  (fromAesonOptions (aesonOptions p))
    { unwrapUnaryRecords = True }

genericDeclareNamedSchema' :: forall a. (Typeable a, ToSample a, ToJSON a, Generic a, GToSchema (Rep a)) => Proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareNamedSchema' proxy =
    genericDeclareNamedSchema
        (schemaOptions Nothing)
        proxy
      & mapped.schema.example ?~ (toJSON :: a -> Value) (head $ map snd $ toSamples (Proxy @a))

genericDeclareNamedSchemaPrefix :: forall a. (Typeable a, ToSample a, ToJSON a, Generic a, GToSchema (Rep a)) => String -> Proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareNamedSchemaPrefix prefix proxy =
    genericDeclareNamedSchema
        (schemaOptions (Just prefix))
        proxy
      & mapped.schema.example ?~ (toJSON :: a -> Value) (head $ map snd $ toSamples (Proxy @a))
