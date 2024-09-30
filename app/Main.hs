{-# LANGUAGE ApplicativeDo #-}
module Main where

import Prelude hiding (id)
import Catbox.Function
import Catbox.Internal
import Options.Applicative

import Toml (TomlParseError (..), pretty)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Pandoc as Pandoc
import qualified Toml
import Catbox.Internal.Monad (runCatbox)

main :: IO ()
main = do
  let
    opts = info (catboxParser <**> helper)
      ( fullDesc
     <> progDesc "Process a set of documents using a catbox graph."
     <> header "catbox - document transformation application" )
  Options {..} <- execParser opts

  -- Read the graph
  result <- createCatboxState inputDirectory graphPath
  case result of
    Left errs ->
      traverse_ TIO.putStrLn errs
    Right initialState ->
      case Map.lookup graphPath (catboxGraphs initialState) of
        Nothing ->
          TIO.putStrLn "Failed to load graphs"

        Just graph -> do
          -- Execute graph and print result
          let
            result = processGraph graphPath standardFunctions graph initialState
          case result of
            Left errs -> do
              TIO.putStrLn ("FAILED! " <> errs)
            Right finalState -> do
              processResults outputDirectory finalState

createCatboxState :: FilePath -> FilePath -> IO (Either [Text] CatboxState)
createCatboxState inputDirectory graphPath = do
  let catboxResults = Map.empty

  paths <- listFilesRecursive inputDirectory ""
  catboxFiles <- loadFiles inputDirectory paths

  result <- loadGraphs graphPath
  case result of
    Left errs ->
      pure (Left errs)
    Right catboxGraphs ->
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

loadGraphs :: FilePath -> IO (Either [Text] (Map FilePath Graph))
loadGraphs path = do
  -- Load this graph
  file <- TIO.readFile path
  case Toml.decode graphCodec file of
    Left msgs -> do
      pure (Left [Toml.prettyTomlDecodeErrors msgs])
    Right graph -> do

      -- Find dependencies
      let
        extractGraph node =
          case nodeType node of
            NodeGraph p ->
              Just (FilePath.combine (FilePath.takeDirectory path) p)
            _ -> Nothing
        dependencies = mapMaybe extractGraph (graphNodes graph)

      results <- traverse loadGraphs dependencies
      case partitionEithers results of
        ([], graphs) ->
          pure (Right (Map.unions (Map.singleton path graph:graphs)))
        (errs, _) ->
          pure (Left (concat errs))

-- Write results to disk and buffer only if all outputs are available.
processResults :: FilePath -> Map Text Value -> IO ()
processResults outputDirectory outputs = do
  traverse_ (processResult outputDirectory) (Map.toList outputs)

processResult :: FilePath -> (Text, Value) -> IO ()
processResult outputDirectory (outputName, value) = do
  let
    printDebug value =
      TIO.putStrLn (outputName <> " = " <> T.pack (show value))

  case value of
    CText value -> printDebug value
    CFilePath value -> printDebug value
    CPandoc value -> printDebug value

    CFile (File path text) -> do
      let
        outputPath = FilePath.combine outputDirectory path
      Directory.createDirectoryIfMissing
        True
        (FilePath.takeDirectory outputPath)
      TIO.writeFile outputPath text

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
