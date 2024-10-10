{-# LANGUAGE ApplicativeDo #-}
module Main where

import Prelude hiding (id)
import Catbox.Function
import Catbox.Internal
import Options.Applicative

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  let
    opts = info (catboxParser <**> helper)
      ( fullDesc
     <> progDesc "Process a set of documents using a catbox graph."
     <> header "catbox - document transformation application" )
  Options {..} <- execParser opts

  -- Read the graph
  result <- createCatboxState inputDirectory graphPath standardFunctions
  case result of
    Left errs ->
      traverse_ TIO.putStrLn errs
    Right initialState ->
      -- Execute graph and print result
      case processGraph (catboxGraph initialState) initialState of
        Left errs -> do
          TIO.putStrLn ("FAILED! " <> errs)
        Right finalState -> do
          processResults outputDirectory finalState

createCatboxState :: FilePath -> FilePath -> Map Text Function -> IO (Either [Text] CatboxState)
createCatboxState inputDirectory graphPath catboxFunctions = do
  let
    catboxResults = Map.empty

  paths <- listFilesRecursive inputDirectory ""
  catboxFiles <- loadFiles inputDirectory paths

  result <- loadGraph graphPath
  case result of
    Left errs ->
      pure (Left errs)
    Right catboxGraph ->
      pure (Right CatboxState { .. })

listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive base path = do
  let
    currentDir = FilePath.combine base path

    collectEntries :: FilePath -> IO [FilePath]
    collectEntries entry = do
      let baseRelativePath = FilePath.combine path entry
      isDir <- Directory.doesDirectoryExist (FilePath.combine currentDir entry)
      if isDir
      then listFilesRecursive base baseRelativePath
      else pure [baseRelativePath]

  entries <- Directory.listDirectory currentDir
  concat <$> traverse collectEntries entries

loadFiles :: FilePath -> [FilePath] -> IO (Map FilePath Text)
loadFiles inputPath paths = do
  let
    loadFile :: FilePath -> IO (FilePath, Text)
    loadFile path = do
      contents <- TIO.readFile (FilePath.combine inputPath path)
      pure (path, contents)

  Map.fromList <$> traverse loadFile paths

loadGraph :: FilePath -> IO (Either [Text] Graph)
loadGraph path = do
  -- Load this graph
  file <- BSL.readFile path
  case Aeson.eitherDecode file of
    Left msg -> do
      pure (Left [T.pack msg])
    Right rawGraph -> do
      resolveRawGraph path rawGraph

resolveRawGraph :: FilePath -> RawGraph -> IO (Either [Text] Graph)
resolveRawGraph path rawGraph = do
  let
    resolveImports :: (Key, Import) -> IO (Either [Text] (Key, Graph))
    resolveImports (key, pathOrGraph) =
      case pathOrGraph of
        Import (Left p) -> do
          result <- loadGraph (FilePath.combine (FilePath.takeDirectory path) p)
          case result of
            Left err ->
              pure (Left err)
            Right g ->
              pure (Right (key, g))
        Import (Right rg) -> do
          result <- resolveRawGraph path rg
          case result of
            Left err ->
              pure (Left err)
            Right g ->
              pure (Right (key, g))

  importResult <- traverse resolveImports (Map.toList (rawGraphImports rawGraph))
  case partitionEithers importResult of
    ([], imports) -> do
      pure (
        Right Graph
          { graphImports = Map.fromList imports
          , graphInputs = rawGraphInputs rawGraph
          , graphNodes = rawGraphNodes rawGraph
          , graphOutputs = rawGraphOutputs rawGraph
          }
        )
    (errs, _) ->
      pure (Left (concat errs))

-- Write results to disk and buffer only if all outputs are available.
processResults :: FilePath -> Map Text Value -> IO ()
processResults outputDirectory outputs = do
  traverse_ (processResult outputDirectory) (Map.toList outputs)

processResult :: FilePath -> (Text, Value) -> IO ()
processResult outputDirectory (outputName, v) = do
  let
    printDebug :: (Show a) => a -> IO ()
    printDebug a =
      TIO.putStrLn (outputName <> " = " <> T.pack (show a))

  -- Print the results to standard out
  case v of
    CFile _ -> pure ()
    CGraph a -> printDebug a
    CList a -> printDebug a
    CPandoc a -> printDebug a
    CPath a -> printDebug a
    CText a -> printDebug a

  -- Write results to filesystem
  case v of
    CFile (File path text) -> do
      let
        outputPath = FilePath.combine outputDirectory path
      Directory.createDirectoryIfMissing
        True
        (FilePath.takeDirectory outputPath)
      TIO.writeFile outputPath text
    CGraph _ -> pure ()
    CList _ -> pure ()
    CPandoc _ -> pure ()
    CPath _ -> pure ()
    CText _ -> pure ()

-------------------------------------------------------------------------------
-- Command line parsing
-------------------------------------------------------------------------------

data Options =
  Options
    { graphPath :: FilePath
    , inputDirectory :: FilePath
    , outputDirectory :: FilePath
    }

catboxParser :: Parser Options
catboxParser = do
  graphPath <- argument str (metavar "GRAPH")
  inputDirectory <-
    strOption
      (  long "input"
      <> metavar "PATH"
      <> help "The path to the input directory."
      )
  outputDirectory <-
    strOption
      (  long "output"
      <> metavar "PATH"
      <> help "The path to the output directory."
      )
  pure Options { .. }
