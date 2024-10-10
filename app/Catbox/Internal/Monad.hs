module Catbox.Internal.Monad
( Catbox
, throwError
, tryError

-- State
, CatboxState(..)
, evalCatbox
, runCatbox

-- Files
, getFilePaths
, getFileContents

-- Graphs
, getGraph
, getImport

-- Results
, insertKey
, resolveParameters
, resolveParameter

-- Functions
, Function(..)
, getFunction
, getFunctions
, fileParam
, graphParam
, pathParam
, pandocParam
, textParam
) where

import Catbox.Internal.Types
import Control.Monad.Except (MonadError, throwError, tryError)
import Text.Pandoc (Pandoc)
import qualified Data.Map as Map
import qualified Data.Text as T

newtype Catbox e a = Catbox (ExceptT e (StateT CatboxState Identity) a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    )
  deriving
    ( MonadState CatboxState
    , MonadError e
    )

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data CatboxState =
  CatboxState
    -- A map of file paths relative to the input folder to file contents.
    { catboxFiles :: Map FilePath Text
    -- The graph that is being executed.
    , catboxGraph :: Graph
    -- The functions that can be called.
    , catboxFunctions :: Map Text Function
    -- Stores the result for each key in the node graph.
    , catboxResults :: Map Key Value
    }

evalCatbox :: Catbox e a -> CatboxState -> Either e a
evalCatbox (Catbox catbox) results =
  runIdentity . flip evalStateT results $ runExceptT catbox

runCatbox :: Catbox e a -> CatboxState -> (Either e a, CatboxState)
runCatbox (Catbox catbox) results =
  runIdentity . flip runStateT results $ runExceptT catbox

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

getFilePaths :: FilePath -> Catbox e [FilePath]
getFilePaths path = do
  files <- gets catboxFiles
  pure (filter (path `isPrefixOf`) (Map.keys files))

getFileContents :: FilePath -> Catbox Text Text
getFileContents path = do
  files <- gets catboxFiles
  case Map.lookup path files of
    Nothing -> throwError ("Cannot find file \"" <> T.pack path <> "\"")
    Just file -> pure file

-------------------------------------------------------------------------------
-- Graphs
-------------------------------------------------------------------------------

getGraph :: Catbox e Graph
getGraph = gets catboxGraph

getImport :: Key -> Catbox Text Graph
getImport key = do
  graph <- getGraph
  case Map.lookup key (graphImports graph) of
    Nothing -> throwError ("Cannot find graph \"" <> keyToText key <> "\"")
    Just file -> pure file

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

data Function =
  Function
    { functionName :: Text
    , functionExec :: Map Text Value -> Key -> Catbox Text ()
    }

getFunction :: Text -> Catbox Text Function
getFunction name = do
  functions <- gets catboxFunctions
  case Map.lookup name functions of
    Nothing -> throwError ("Cannot find function \"" <> name <> "\"")
    Just fn -> pure fn

getFunctions :: Catbox e (Map Text Function)
getFunctions = gets catboxFunctions

fileParam :: Text -> Map Text Value -> Catbox Text File
fileParam name params =
  case Map.lookup name params of
    Just (CFile v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a file")
    _ -> throwError ("Cannot find parameter " <> name)

graphParam :: Text -> Map Text Value -> Catbox Text Graph
graphParam name params =
  case Map.lookup name params of
    Just (CGraph v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a graph")
    _ -> throwError ("Cannot find parameter " <> name)

pathParam :: Text -> Map Text Value -> Catbox Text FilePath
pathParam name params =
  case Map.lookup name params of
    Just (CPath v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a path")
    _ -> throwError ("Cannot find parameter " <> name)

pandocParam :: Text -> Map Text Value -> Catbox Text Pandoc
pandocParam name params =
  case Map.lookup name params of
    Just (CPandoc v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a pandoc")
    _ -> throwError ("Cannot find parameter " <> name)

textParam :: Text -> Map Text Value -> Catbox Text Text
textParam name params =
  case Map.lookup name params of
    Just (CText v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a text")
    _ -> throwError ("Cannot find parameter " <> name)

-------------------------------------------------------------------------------
-- Results
-------------------------------------------------------------------------------

insertKey :: Key -> Value -> Catbox e ()
insertKey key value = do
  s <- get
  put s { catboxResults = Map.insert key value (catboxResults s) }

resolveParameters :: [Parameter] -> Catbox Text (Map Text Value)
resolveParameters parameters = do
  results <- traverse resolveParameter parameters
  pure (Map.fromList results)

resolveParameter :: Parameter -> Catbox Text (Text, Value)
resolveParameter parameter = do
  case parameterSource parameter of
    Constant c ->
      pure (parameterName parameter, c)
    Connection key -> do
      results <- gets catboxResults
      case Map.lookup key results of
        Nothing ->
          throwError ("Cannot find " <> keyToText key)
        Just value ->
          pure (parameterName parameter, value)
