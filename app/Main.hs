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
  Options {..} <- firstExecParser opts

  -- Read the graph
  file <- TIO.readFile graphPath
  case Toml.decode graphCodec file of
    Left msgs ->
      TIO.putStrLn (Toml.prettyTomlDecodeErrors msgs)

    -- Create state
    Right graph -> do
      initialState <- createCatboxState inputDirectory graph

      -- Execute graph and print result
      let (result, finalState) = runCatbox (processNodes (graphNodes graph)) initialState
      case result of
        Left errs -> do
          TIO.putStrLn ("FAILED! " <> errs)
        Right () -> do
          processResults outputDirectory finalState (graphOutputs graph)

createCatboxState :: FilePath -> Graph -> IO CatboxState
createCatboxState inputDirectory graph = do
  let
    inputs = graphInputs graph
    graphOpts = info (catboxParser <**> inputsParser inputs <**> helper) fullDesc
  catboxResults <- execParser graphOpts

  paths <- listFilesRecursive inputDirectory ""
  catboxFiles <- loadFiles inputDirectory paths

  pure CatboxState { .. }

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

-- Write results to disk and buffer only if all outputs are available.
processResults :: FilePath -> CatboxState -> [Output] -> IO ()
processResults outputDirectory state outputs = do
  let
    loadResult :: Output -> Either Text (Output, Value)
    loadResult output =
      (\(_, v) -> (output, v)) <$>
        evalCatbox (resolveParameter (outputParameter output)) state

  case partitionEithers (loadResult <$> outputs) of

    -- All of the results are available, print/write the results
    ([], successfulReturns) -> do
      TIO.putStrLn "SUCCESS!"
      traverse_ (processResult outputDirectory) successfulReturns

    -- Some results are missing!
    (failedOutputs, _) -> do
      TIO.putStrLn $
           "Could not find results for: "
        <> T.intercalate ", " (T.pack . show <$> failedOutputs)

processResult :: FilePath -> (Output, Value) -> IO ()
processResult outputDirectory (output, value) = do
  let
    printDebug value =
      TIO.putStrLn (outputName output <> " = " <> T.pack (show value))

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

-- We only want to consider the first two positional arguments the first time
-- we parse the command line options, because we don't know what the flags are
-- until the graph is read. This is because we generate flags based on what the
-- input values for the graph are.
firstExecParser :: ParserInfo a -> IO a
firstExecParser pinfo = do
  args <- getArgs
  handleParseResult $
    execParserPure defaultPrefs pinfo (take 5 args)

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

inputsParser :: [Input] -> Parser (Options -> Results)
inputsParser inputs = do
  results <- traverse inputParser inputs
  pure (const $ Map.unions results)

inputParser :: Input -> Parser Results
inputParser input =
  case inputType input of
    "text" -> do
      value <- strOption
          ( long (T.unpack (inputName input))
         <> metavar "VALUE"
         <> help "string value" )
      pure $
        Map.singleton
          (Key ("in." <> inputName input))
          (CText value)
    "path" -> do
      path <- strOption
          ( long (T.unpack (inputName input))
         <> metavar "VALUE"
         <> help "path" )
      pure $
        Map.singleton
          (Key ("in." <> inputName input))
          (CFilePath path)
    "file" -> do
      path <- strOption
          ( long (T.unpack (inputName input))
         <> metavar "VALUE"
         <> help "path" )
      pure $
        Map.singleton
          (Key ("in." <> inputName input))
          (CFile (File path "")) -- Read the file later

-------------------------------------------------------------------------------
-- Program functions
-------------------------------------------------------------------------------

-- Tries to process the output of all nodes in the graph. Returns an error if
-- it fails to do so.
processNodes :: [Node] -> Catbox Text ()
processNodes nodes = do
  let
    tryExec :: [(Node, Text)] -> Node -> Catbox Text [(Node, Text)]
    tryExec failed node = do
      result <- tryError (processNode node)
      case result of
        Left err -> pure ((node, err):failed)
        Right () -> pure failed

  failed <- foldlM tryExec [] nodes

  case failed of
    -- Finished processing all nodes
    [] -> pure ()

    (node, e):_
      -- We were unable to process any of the nodes.
      | length failed == length nodes ->
        throwError e -- TODO: Return all of the errors

      -- Some succeeded, try again
      | otherwise ->
        processNodes (fst <$> failed)

processNode :: Node -> Catbox Text ()
processNode node = do
  args <- resolveParameters (nodeParameters node)
  invoke
    (baseFunctions <> pandocFunctions)
    (nodeFunction node)
    args
    (Key (nodeId node))
