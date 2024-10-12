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
    , functionApiInputs :: Map Text ValueType
    , functionApiOutputs :: Map Text ValueType
    , functionApiVariableInputs :: Bool
    , functionApiVariableOutputs :: Bool
    }

instance Aeson.FromJSON FunctionApi where
  parseJSON = Aeson.withObject "FunctionApi" $ \v -> do
    FunctionApi
      <$> v .: "name"
      <*> v .: "inputs"
      <*> v .: "outputs"
      <*> v .: "variable_inputs"
      <*> v .: "variable_outputs"

instance Aeson.ToJSON FunctionApi where
  toJSON v =
    Aeson.object
      [ "name" .= functionApiName v
      , "inputs" .= functionApiInputs v
      , "outputs" .= functionApiOutputs v
      , "variable_inputs" .= functionApiVariableInputs v
      , "variable_outputs" .= functionApiVariableOutputs v
      ]

extractFunctionApis :: [Function] -> [FunctionApi]
extractFunctionApis = fmap extractFunctionApi

extractFunctionApi :: Function -> FunctionApi
extractFunctionApi Function {..} =
  FunctionApi
    { functionApiName = functionName
    , functionApiInputs = functionInputs
    , functionApiOutputs = functionOutputs
    , functionApiVariableInputs = functionVariableInputs
    , functionApiVariableOutputs = functionVariableOutputs
    }
