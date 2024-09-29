module Catbox.Internal.Types
( Graph(graphInputs, graphNodes, graphOutputs)
, graphCodec

-- Parts of the graph
, Parameter(parameterName, parameterType)
, Node(nodeId, nodeFunction, nodeConnections)
, Return(returnName, returnConnection)

-- Primitive types used by the graph
, Results(..)
, Key(..)
, Value(..)
, CatboxFile(..)
) where

import Toml (TomlCodec, (.=))
import qualified Toml

data Graph =
  Graph
    { graphInputs :: [Parameter]
    , graphNodes :: [Node]
    , graphOutputs :: [Return]
    }

graphCodec :: TomlCodec Graph
graphCodec =
  Graph
    <$> Toml.list parameterCodec "in" .= graphInputs
    <*> Toml.list nodeCodec "nodes" .= graphNodes
    <*> Toml.list returnCodec "out" .= graphOutputs

-------------------------------------------------------------------------------
-- Graph Parts
-------------------------------------------------------------------------------

data Parameter =
  Parameter
    { parameterName :: Text
    , parameterType :: Text
    }

parameterCodec :: TomlCodec Parameter
parameterCodec =
  Parameter
    <$> Toml.text "name" .= parameterName
    <*> Toml.text "type" .= parameterType

data Node =
  Node
    { nodeId :: Text
    , nodeFunction :: Text
    , nodeConnections :: [Key]
    }

nodeCodec :: TomlCodec Node
nodeCodec =
  Node
    <$> Toml.text "id" .= nodeId
    <*> Toml.text "function" .= nodeFunction
    <*> Toml.arrayOf (Toml._Coerce Toml._Text) "in" .= nodeConnections

data Return =
  Return
    { returnName :: Text
    , returnConnection :: Key
    }

returnCodec :: TomlCodec Return
returnCodec =
  Return
    <$> Toml.text "name" .= returnName
    <*> (Key <$> Toml.text "from" .= (keyToText . returnConnection))

-------------------------------------------------------------------------------
-- Graph Primitive Types
-------------------------------------------------------------------------------

-- Stores the result for each key in the node graph.
type Results = Map Key Value

-- Represents the key for a result in a node graph.
newtype Key = Key { keyToText :: Text }
  deriving newtype (Eq, IsString, Ord, Show)

-- The different kinds of values you can pass in catbox.
data Value =
    CText Text
  | CFilePath FilePath
  | CFile CatboxFile
  deriving (Eq, Show)

data CatboxFile =
  CatboxFile
    { filePath :: FilePath
    , fileText :: Text
    }
  deriving (Eq, Show)
