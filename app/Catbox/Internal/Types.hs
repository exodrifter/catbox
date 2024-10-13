{-# LANGUAGE DerivingVia #-}
module Catbox.Internal.Types
( RawGraph(..)
, Graph(..)
, Import(..)

-- Parts of the graph
, Signature(..)
, Node(nodeId, nodeFunction)
, Parameter(parameterKey, parameterSource)
, ParameterSource(..)

-- Primitive types used by the graph
, Results
, Key, keyIdPart, keyFieldPart, keyFromText, keyToText
, Value(..)
, ValueType(..)
, File(..)
, Object(..)
) where

import Data.Aeson ((.:), (.:?), (.=), (.!=))
import Text.Pandoc (Pandoc)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import qualified Data.Text as T

data RawGraph =
  RawGraph
    { rawGraphImports :: Map Text Import
    , rawGraphInputs :: [Signature]
    , rawGraphOutputs :: [Signature]
    , rawGraphNodes :: [Node]
    , rawGraphParameters :: [Parameter]
    }
  deriving (Eq, Show)

instance Aeson.FromJSON RawGraph where
  parseJSON = Aeson.withObject "RawGraph" $ \v -> do
    RawGraph
      <$> v .:? "imports" .!= Map.empty
      <*> v .: "inputs"
      <*> v .: "outputs"
      <*> v .: "nodes"
      <*> v .: "parameters"

instance Aeson.ToJSON RawGraph where
  toJSON v =
    Aeson.object
      [ "imports" .= rawGraphImports v
      , "inputs" .= rawGraphInputs v
      , "outputs" .= rawGraphOutputs v
      , "nodes" .= rawGraphNodes v
      , "parameters" .= rawGraphParameters v
      ]

newtype Import = Import (Either FilePath RawGraph)
  deriving (Eq, Show)

instance Aeson.FromJSON Import where
  parseJSON v =
    let
      graphParser = do
        graph <- Aeson.parseJSON v
        pure (Import (Right graph))
      textParser = do
        text <- Aeson.parseJSON v
        pure (Import (Left text))
    in
      textParser <|> graphParser

instance Aeson.ToJSON Import where
  toJSON v =
    case v of
      Import (Left path) -> Aeson.toJSON path
      Import (Right rawGraph) -> Aeson.toJSON rawGraph

-- The same as RawGraph, but with the imports resolved.
data Graph =
  Graph
    { graphImports :: Map Text Graph
    , graphInputs :: [Signature]
    , graphOutputs :: [Signature]
    , graphNodes :: [Node]
    , graphParameters :: [Parameter]
    }
  deriving (Eq, Show)

instance Aeson.FromJSON Graph where
  parseJSON = Aeson.withObject "Graph" $ \v -> do
    Graph
      <$> v .:? "imports" .!= Map.empty
      <*> v .: "inputs"
      <*> v .: "outputs"
      <*> v .: "nodes"
      <*> v .: "parameters"

instance Aeson.ToJSON Graph where
  toJSON v =
    Aeson.object
      [ "imports" .= graphImports v
      , "inputs" .= graphInputs v
      , "outputs" .= graphOutputs v
      , "nodes" .= graphNodes v
      , "parameters" .= graphParameters v
      ]

-------------------------------------------------------------------------------
-- Graph Parts
-------------------------------------------------------------------------------

data Signature =
  Signature
    { signatureName :: Text
    , signatureType :: ValueType
    }
  deriving (Eq, Show)

instance Aeson.FromJSON Signature where
  parseJSON = Aeson.withObject "Signature" $ \v -> do
    Signature
      <$> v .: "name"
      <*> v .: "type"

instance Aeson.ToJSON Signature where
  toJSON v =
    Aeson.object
      [ "name" .= signatureName v
      , "type" .= signatureType v
      ]

data Node =
  Node
    { nodeId :: Text
    , nodeFunction :: Text
    }
  deriving (Eq, Show)

instance Aeson.FromJSON Node where
  parseJSON = Aeson.withObject "Node" $ \v -> do
    Node
      <$> v .: "id"
      <*> v .: "function"

instance Aeson.ToJSON Node where
  toJSON v =
    Aeson.object
      [ "id" .= nodeId v
      , "function" .= nodeFunction v
      ]

data Parameter =
  Parameter
    { parameterKey :: Key
    , parameterSource :: ParameterSource
    }
  deriving (Eq, Show)

instance Aeson.FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \v -> do
    Parameter
      <$> v .: "key"
      <*> v .: "source"

instance Aeson.ToJSON Parameter where
  toJSON v =
    Aeson.object
      [ "key" .= parameterKey v
      , "source" .= parameterSource v
      ]

data ParameterSource =
    Connection Key
  | Constant Value
  deriving (Eq, Show)

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

-------------------------------------------------------------------------------
-- Graph Primitive Types
-------------------------------------------------------------------------------

-- Stores the result for each key in the node graph.
type Results = Map Key Value

-- Represents the key for a result in a node graph.
newtype Key = Key [Text]
  deriving newtype (Eq, Ord, Semigroup, Show)

instance IsString Key where
  fromString = keyFromText . T.pack

instance Aeson.FromJSON Key where
  parseJSON = Aeson.withText "Key" $ pure . keyFromText

instance Aeson.ToJSON Key where
  toJSON = Aeson.String . keyToText

keyIdPart :: Key -> Maybe Text
keyIdPart (Key parts) =
  case parts of
    x:_ -> Just x
    _ -> Nothing

keyFieldPart :: Key -> Maybe Text
keyFieldPart (Key parts) =
  case parts of
    _:x:[] -> Just x
    _ -> Nothing

keyFromText :: Text -> Key
keyFromText = Key . T.split (== '.')

keyToText :: Key -> Text
keyToText (Key parts) = T.intercalate "." parts

-- The different kinds of values you can pass in catbox.
data Value =
    CFile File
  | CGraph Graph
  | CList [Value]
  | CObject Object
  | CPandoc Pandoc
  | CPath FilePath
  | CText Text
  deriving (Eq, Show)

instance Aeson.FromJSON Value where
  parseJSON = Aeson.withObject "Value" $ \v -> do
    typ <- v .: "type" :: Aeson.Parser ValueType
    case typ of
      TFile -> CFile <$> v .: "value"
      TGraph -> CGraph <$> v .: "value"
      TList -> CList <$> v .: "value"
      TObject -> CObject <$> v .: "value"
      TPandoc -> CPandoc <$> v .: "value"
      TPath -> CPath <$> v .: "value"
      TText -> CText <$> v .: "value"

instance Aeson.ToJSON Value where
  toJSON v =
    case v of
      CFile a ->
        Aeson.object
          [ "type" .= TFile
          , "value" .= a
          ]
      CGraph a ->
        Aeson.object
          [ "type" .= TGraph
          , "value" .= a
          ]
      CList a ->
        Aeson.object
          [ "type" .= TList
          , "value" .= a
          ]
      CObject a ->
        Aeson.object
          [ "type" .= TObject
          , "value" .= a
          ]
      CPandoc a ->
        Aeson.object
          [ "type" .= TPandoc
          , "value" .= a
          ]
      CPath a ->
        Aeson.object
          [ "type" .= TPath
          , "value" .= a
          ]
      CText a ->
        Aeson.object
          [ "type" .= TText
          , "value" .= a
          ]

data ValueType =
    TFile
  | TGraph
  | TList
  | TObject
  | TPandoc
  | TPath
  | TText
  deriving (Eq, Show)

instance Aeson.FromJSON ValueType where
  parseJSON = Aeson.withText "Value" $ \typ -> do
    case typ of
      "file" -> pure TFile
      "graph" -> pure TGraph
      "list" -> pure TList
      "object" -> pure TObject
      "pandoc" -> pure TPandoc
      "path" -> pure TPath
      "text" -> pure TText
      _ -> fail (T.unpack ("unknown value type \"" <> typ <> "\""))


instance Aeson.ToJSON ValueType where
  toJSON v =
    case v of
      TFile -> Aeson.String "file"
      TGraph -> Aeson.String "graph"
      TList -> Aeson.String "list"
      TObject -> Aeson.String "object"
      TPandoc -> Aeson.String "pandoc"
      TPath -> Aeson.String "path"
      TText -> Aeson.String "text"

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

newtype Object = Object { objectFields :: Map Text Value }
  deriving newtype (Eq, Show)
  deriving Aeson.FromJSON via Map Text Value
  deriving Aeson.ToJSON via Map Text Value
