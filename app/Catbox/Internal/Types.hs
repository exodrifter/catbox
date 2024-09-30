module Catbox.Internal.Types
( Graph(graphInputs, graphNodes, graphOutputs)
, graphCodec

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

import Text.Pandoc (Pandoc)
import Toml (TomlCodec, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Encoding as TE
import qualified Toml

data Graph =
  Graph
    { graphInputs :: [Input]
    , graphNodes :: [Node]
    , graphOutputs :: [Output]
    }

graphCodec :: TomlCodec Graph
graphCodec =
  Graph
    <$> Toml.list inputCodec "in" .= graphInputs
    <*> Toml.list nodeCodec "nodes" .= graphNodes
    <*> Toml.list outputCodec "out" .= graphOutputs

-------------------------------------------------------------------------------
-- Graph Parts
-------------------------------------------------------------------------------

data Input =
  Input
    { inputName :: Text
    , inputType :: Text
    }

inputCodec :: TomlCodec Input
inputCodec =
  Input
    <$> Toml.text "name" .= inputName
    <*> Toml.text "type" .= inputType

data Node =
  Node
    { nodeId :: Text
    , nodeType :: NodeType
    , nodeParameters :: [Parameter]
    }

nodeCodec :: TomlCodec Node
nodeCodec =
  Node
    <$> Toml.text "id" .= nodeId
    <*> Toml.table nodeTypeCodec "type" .= nodeType
    <*> Toml.list parameterCodec "parameters" .= nodeParameters

data NodeType =
    NodeFunction Text
  | NodeGraph FilePath

nodeTypeCodec :: TomlCodec NodeType
nodeTypeCodec =
  let
    matchFunction (NodeFunction name) = Just name
    matchFunction _ = Nothing

    matchGraph (NodeGraph path) = Just path
    matchGraph _ = Nothing

  in    Toml.dimatch matchFunction NodeFunction (Toml.text "function")
    <|> Toml.dimatch matchGraph NodeGraph (Toml.string "graph")

data Parameter =
  Parameter
    { parameterName :: Text
    , parameterSource :: ParameterSource
    }

parameterCodec :: TomlCodec Parameter
parameterCodec =
  Parameter
    <$> Toml.text "name" .= parameterName
    <*> parameterSourceCodec .= parameterSource

data ParameterSource =
    Connection Key
  | Constant Value

parameterSourceCodec :: TomlCodec ParameterSource
parameterSourceCodec =
  let
    matchConnection (Connection key) = Just key
    matchConnection _ = Nothing

    matchConstant (Constant value) = Just value
    matchConstant _ = Nothing

  in    Toml.dimatch matchConstant Constant valueCodec
    <|> Toml.dimatch matchConnection Connection
          (Toml.textBy keyToText (Right . Key) "key")

data Output =
  Output
    { outputName :: Text
    , outputParameter :: Parameter
    }

outputCodec :: TomlCodec Output
outputCodec =
  Output
    <$> Toml.text "name" .= outputName
    <*> parameterCodec .= outputParameter

-------------------------------------------------------------------------------
-- Graph Primitive Types
-------------------------------------------------------------------------------

-- Stores the result for each key in the node graph.
type Results = Map Key Value

-- Represents the key for a result in a node graph.
newtype Key = Key { keyToText :: Text }
  deriving newtype (Eq, IsString, Ord, Semigroup, Show)

-- The different kinds of values you can pass in catbox.
data Value =
    CFile File
  | CFilePath FilePath
  | CPandoc Pandoc
  | CText Text
  deriving (Eq, Show)

valueCodec :: TomlCodec Value
valueCodec =
  let
    matchCFile (CFile v) = Just v
    matchCFile _ = Nothing

    matchCFilePath (CFilePath v) = Just v
    matchCFilePath _ = Nothing

    matchCPandoc (CPandoc v) = Just v
    matchCPandoc _ = Nothing

    matchCText (CText v) = Just v
    matchCText _ = Nothing

  in    Toml.dimatch matchCFile CFile (Toml.table fileCodec "file")
    <|> Toml.dimatch matchCFilePath CFilePath (Toml.string "path")
    <|> Toml.dimatch matchCPandoc CPandoc pandocCodec
    <|> Toml.dimatch matchCText CText (Toml.text "text")

pandocCodec :: TomlCodec Pandoc
pandocCodec =
  let
    toText pandoc = TL.toStrict (TL.toLazyText (Aeson.encodeToTextBuilder pandoc))
    fromText text =
      case Aeson.eitherDecode (BSL.fromStrict (TE.encodeUtf8 text)) of
        Left err -> Left (T.pack err)
        Right pandoc -> Right pandoc
  in
    Toml.textBy toText fromText "pandoc"

data File =
  File
    { filePath :: FilePath
    , fileText :: Text
    }
  deriving (Eq, Show)

fileCodec :: TomlCodec File
fileCodec =
  File
    <$> Toml.string "path" .= filePath
    <*> Toml.text "text" .= fileText
