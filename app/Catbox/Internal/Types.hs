{-# LANGUAGE DerivingVia #-}
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
import Data.Aeson ((.:))
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
      [ "inputs" Aeson..= graphInputs v
      , "nodes" Aeson..= graphNodes v
      , "outputs" Aeson..= graphOutputs v
      ]

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

instance Aeson.FromJSON Input where
  parseJSON = Aeson.withObject "Input" $ \v -> do
    Input
      <$> v .: "name"
      <*> v .: "type"

instance Aeson.ToJSON Input where
  toJSON v =
    Aeson.object
      [ "name" Aeson..= inputName v
      , "type" Aeson..= inputType v
      ]

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

instance Aeson.FromJSON Node where
  parseJSON = Aeson.withObject "Node" $ \v -> do
    Node
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "parameters"

instance Aeson.ToJSON Node where
  toJSON v =
    Aeson.object
      [ "id" Aeson..= nodeId v
      , "type" Aeson..= nodeType v
      , "parameters" Aeson..= nodeParameters v
      ]

nodeCodec :: TomlCodec Node
nodeCodec =
  Node
    <$> Toml.text "id" .= nodeId
    <*> Toml.table nodeTypeCodec "type" .= nodeType
    <*> Toml.list parameterCodec "parameters" .= nodeParameters

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
          [ "type" Aeson..= ("function" :: Text)
          , "value" Aeson..= a
          ]
      NodeGraph a ->
        Aeson.object
          [ "type" Aeson..= ("graph" :: Text)
          , "value" Aeson..= a
          ]

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

instance Aeson.FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \v -> do
    Parameter
      <$> v .: "name"
      <*> v .: "source"

instance Aeson.ToJSON Parameter where
  toJSON v =
    Aeson.object
      [ "name" Aeson..= parameterName v
      , "source" Aeson..= parameterSource v
      ]

parameterCodec :: TomlCodec Parameter
parameterCodec =
  Parameter
    <$> Toml.text "name" .= parameterName
    <*> parameterSourceCodec .= parameterSource

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
          [ "type" Aeson..= ("connection" :: Text)
          , "value" Aeson..= a
          ]
      Constant a ->
        Aeson.object
          [ "type" Aeson..= ("constant" :: Text)
          , "value" Aeson..= a
          ]

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

instance Aeson.FromJSON Output where
  parseJSON = Aeson.withObject "Output" $ \v -> do
    Output
      <$> v .: "name"
      <*> v .: "parameter"

instance Aeson.ToJSON Output where
  toJSON v =
    Aeson.object
      [ "name" Aeson..= outputName v
      , "parameter" Aeson..= outputParameter v
      ]

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
          [ "type" Aeson..= ("array" :: Text)
          , "value" Aeson..= a
          ]
      CFile a ->
        Aeson.object
          [ "type" Aeson..= ("file" :: Text)
          , "value" Aeson..= a
          ]
      CFilePath a ->
        Aeson.object
          [ "type" Aeson..= ("path" :: Text)
          , "value" Aeson..= a
          ]
      CPandoc a ->
        Aeson.object
          [ "type" Aeson..= ("pandoc" :: Text)
          , "value" Aeson..= a
          ]
      CText a ->
        Aeson.object
          [ "type" Aeson..= ("text" :: Text)
          , "value" Aeson..= a
          ]

valueCodec :: TomlCodec Value
valueCodec =
  let
    matchCArray (CArray v) = Just v
    matchCArray _ = Nothing

    matchCFile (CFile v) = Just v
    matchCFile _ = Nothing

    matchCFilePath (CFilePath v) = Just v
    matchCFilePath _ = Nothing

    matchCPandoc (CPandoc v) = Just v
    matchCPandoc _ = Nothing

    matchCText (CText v) = Just v
    matchCText _ = Nothing

  in    Toml.dimatch matchCArray CArray (Toml.list valueCodec "array")
    <|> Toml.dimatch matchCFile CFile (Toml.table fileCodec "file")
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

instance Aeson.FromJSON File where
  parseJSON = Aeson.withObject "File" $ \v ->
    File
      <$> v .: "path"
      <*> v .: "text"

instance Aeson.ToJSON File where
  toJSON v =
    Aeson.object
      [ "path" Aeson..= filePath v
      , "text" Aeson..= fileText v
      ]

fileCodec :: TomlCodec File
fileCodec =
  File
    <$> Toml.string "path" .= filePath
    <*> Toml.text "text" .= fileText
