module Catbox.Internal.Api
( FunctionApi(..)
, extractFunctionApis
, extractFunctionApi
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as Aeson

data FunctionApi =
  FunctionApi
    { functionApiName :: Text
    , functionApiParams :: Map Text ValueType
    , functionApiEnableVariableParams :: Bool
    }

instance Aeson.FromJSON FunctionApi where
  parseJSON = Aeson.withObject "FunctionApi" $ \v -> do
    FunctionApi
      <$> v .: "name"
      <*> v .: "params"
      <*> v .: "variable"

instance Aeson.ToJSON FunctionApi where
  toJSON v =
    Aeson.object
      [ "name" .= functionApiName v
      , "params" .= functionApiParams v
      , "variable" .= functionApiEnableVariableParams v
      ]

extractFunctionApis :: [Function] -> [FunctionApi]
extractFunctionApis = fmap extractFunctionApi

extractFunctionApi :: Function -> FunctionApi
extractFunctionApi Function {..} =
  FunctionApi
    { functionApiName = functionName
    , functionApiParams = functionParams
    , functionApiEnableVariableParams = functionEnableVariableParams
    }
