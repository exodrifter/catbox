{-# LANGUAGE DerivingVia #-}
module Catbox.Internal.Types
( Graph(graphInputs, graphNodes, graphOutputs)

-- Parts of the graph
, Input(inputName, inputType)
, Node(nodeId, nodeType, nodeParameters)
, NodeType(..)
, Output(outputName, outputParameter)
, Parameter(parameterName, parameterSource)
, ParameterSource(..)

-- Primitive types used by the graph
, Results(..)
, Key(..)
, Value(..)
, File(..)
) where

import Data.Aeson ((.:), (.=))
import Text.Pandoc (Pandoc)
import qualified Data.Aeson.Types as Aeson

data Graph =
  Graph
    { graphInputs :: [Input]
    , graphNodes :: [Node]
    , graphOutputs :: [Output]
    }

instance Aeson.FromJSON Graph where
  parseJSON = Aeson.withObject "Graph" $ \v -> do
    Graph
      <$> v .: "inputs"
      <*> v .: "nodes"
      <*> v .: "outputs"

instance Aeson.ToJSON Graph where
  toJSON v =
    Aeson.object
      [ "inputs" .= graphInputs v
      , "nodes" .= graphNodes v
      , "outputs" .= graphOutputs v
      ]

-------------------------------------------------------------------------------
-- Graph Parts
-------------------------------------------------------------------------------

data Input =
  Input
    { inputName :: Text
    , inputType :: Text
    }

instance Aeson.FromJSON Input where
  parseJSON = Aeson.withObject "Input" $ \v -> do
    Input
      <$> v .: "name"
      <*> v .: "type"

instance Aeson.ToJSON Input where
  toJSON v =
    Aeson.object
      [ "name" .= inputName v
      , "type" .= inputType v
      ]

data Node =
  Node
    { nodeId :: Text
    , nodeType :: NodeType
    , nodeParameters :: [Parameter]
    }

instance Aeson.FromJSON Node where
  parseJSON = Aeson.withObject "Node" $ \v -> do
    Node
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "parameters"

instance Aeson.ToJSON Node where
  toJSON v =
    Aeson.object
      [ "id" .= nodeId v
      , "type" .= nodeType v
      , "parameters" .= nodeParameters v
      ]

data NodeType =
    NodeFunction Text
  | NodeGraph FilePath

instance Aeson.FromJSON NodeType where
  parseJSON = Aeson.withObject "NodeType" $ \v -> do
    typ <- v .: "type" :: Aeson.Parser Text
    case typ of
      "function" -> NodeFunction <$> v .: "value"
      "graph" -> NodeGraph <$> v .: "value"
      _ -> fail "unknown node type type"

instance Aeson.ToJSON NodeType where
  toJSON v =
    case v of
      NodeFunction a ->
        Aeson.object
          [ "type" .= ("function" :: Text)
          , "value" .= a
          ]
      NodeGraph a ->
        Aeson.object
          [ "type" .= ("graph" :: Text)
          , "value" .= a
          ]

data Parameter =
  Parameter
    { parameterName :: Text
    , parameterSource :: ParameterSource
    }

instance Aeson.FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \v -> do
    Parameter
      <$> v .: "name"
      <*> v .: "source"

instance Aeson.ToJSON Parameter where
  toJSON v =
    Aeson.object
      [ "name" .= parameterName v
      , "source" .= parameterSource v
      ]

data ParameterSource =
    Connection Key
  | Constant Value

instance Aeson.FromJSON ParameterSource where
  parseJSON = Aeson.withObject "ParameterSource" $ \v -> do
    typ <- v .: "type" :: Aeson.Parser Text
    case typ of
      "connection" -> Connection <$> v .: "value"
      "constant" -> Constant <$> v .: "value"
      _ -> fail "unknown parameter source type"

instance Aeson.ToJSON ParameterSource where
  toJSON v =
    case v of
      Connection a ->
        Aeson.object
          [ "type" .= ("connection" :: Text)
          , "value" .= a
          ]
      Constant a ->
        Aeson.object
          [ "type" .= ("constant" :: Text)
          , "value" .= a
          ]

data Output =
  Output
    { outputName :: Text
    , outputParameter :: Parameter
    }

instance Aeson.FromJSON Output where
  parseJSON = Aeson.withObject "Output" $ \v -> do
    Output
      <$> v .: "name"
      <*> v .: "parameter"

instance Aeson.ToJSON Output where
  toJSON v =
    Aeson.object
      [ "name" .= outputName v
      , "parameter" .= outputParameter v
      ]

-------------------------------------------------------------------------------
-- Graph Primitive Types
-------------------------------------------------------------------------------

-- Stores the result for each key in the node graph.
type Results = Map Key Value

-- Represents the key for a result in a node graph.
newtype Key = Key { keyToText :: Text }
  deriving newtype (Eq, IsString, Ord, Semigroup, Show)
  deriving Aeson.FromJSON via Text
  deriving Aeson.ToJSON via Text

-- The different kinds of values you can pass in catbox.
data Value =
    CArray [Value]
  | CFile File
  | CFilePath FilePath
  | CPandoc Pandoc
  | CText Text
  deriving (Eq, Show)

instance Aeson.FromJSON Value where
  parseJSON = Aeson.withObject "Value" $ \v -> do
    typ <- v .: "type" :: Aeson.Parser Text
    case typ of
      "array" -> CArray <$> v .: "value"
      "file" -> CFile <$> v .: "value"
      "path" -> CFilePath <$> v .: "value"
      "pandoc" -> CPandoc <$> v .: "value"
      "text" -> CText <$> v .: "value"
      _ -> fail "unknown value type"

instance Aeson.ToJSON Value where
  toJSON v =
    case v of
      CArray a ->
        Aeson.object
          [ "type" .= ("array" :: Text)
          , "value" .= a
          ]
      CFile a ->
        Aeson.object
          [ "type" .= ("file" :: Text)
          , "value" .= a
          ]
      CFilePath a ->
        Aeson.object
          [ "type" .= ("path" :: Text)
          , "value" .= a
          ]
      CPandoc a ->
        Aeson.object
          [ "type" .= ("pandoc" :: Text)
          , "value" .= a
          ]
      CText a ->
        Aeson.object
          [ "type" .= ("text" :: Text)
          , "value" .= a
          ]

data File =
  File
    { filePath :: FilePath
    , fileText :: Text
    }
  deriving (Eq, Show)

instance Aeson.FromJSON File where
  parseJSON = Aeson.withObject "File" $ \v ->
    File
      <$> v .: "path"
      <*> v .: "text"

instance Aeson.ToJSON File where
  toJSON v =
    Aeson.object
      [ "path" .= filePath v
      , "text" .= fileText v
      ]
