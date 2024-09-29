{-# LANGUAGE ApplicativeDo #-}
module Main where

import Prelude hiding (id)
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

main :: IO ()
main = do
  let
    opts = info (catboxParser <**> helper)
      ( fullDesc
     <> progDesc "Process a set of documents using a catbox graph."
     <> header "catbox - document transformation application" )
  (graphPath, inputDirectory, outputDirectory) <- firstExecParser opts

  -- Read the graph
  file <- TIO.readFile graphPath
  case Toml.decode graphCodec file of
    Left msgs ->
      TIO.putStrLn (Toml.prettyTomlDecodeErrors msgs)

    -- Read input
    Right graph -> do
      let
        inputs = graphInputs graph
        graphOpts = info (catboxParser <**> inputsParser inputs <**> helper) fullDesc
      rawInputs <- execParser graphOpts

      -- Read the files
      let
        loadFile :: Results -> (Key, Value) -> IO Results
        loadFile results (key, value) =
          case value of
            CFile (File path _) -> do
              text <- TIO.readFile (FilePath.combine inputDirectory path)
              pure (Map.insert key (CFile (File path text)) results)
            CFilePath _ -> pure results
            CPandoc _ -> pure results
            CText _ -> pure results
      inputs <- foldlM loadFile rawInputs (Map.toList rawInputs)

      -- Execute graph and print result
      let (result, newValues) = runCatbox (processNodes (graphNodes graph)) inputs
      case result of
        Left errs -> do
          TIO.putStrLn "FAILED!"
          traverse_ TIO.putStrLn errs
        Right result -> do
          processResults outputDirectory newValues (graphOutputs graph)

-- Write results to disk and buffer only if all outputs are available.
processResults :: FilePath -> Results -> [Output] -> IO ()
processResults outputDirectory results outputs = do
  let
    loadResult :: Output -> Either Text (Output, Value)
    loadResult output =
      (\v -> (output, v)) <$>
        (evalCatbox (resolveParameter (outputParameter output)) results)

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

-- We only want to consider the first two positional arguments the first time
-- we parse the command line options, because we don't know what the flags are
-- until the graph is read. This is because we generate flags based on what the
-- input values for the graph are.
firstExecParser :: ParserInfo a -> IO a
firstExecParser pinfo = do
  args <- getArgs
  handleParseResult $
    execParserPure defaultPrefs pinfo (take 5 args)

catboxParser :: Parser (FilePath, FilePath, FilePath)
catboxParser = do
  graphPath <- argument str (metavar "GRAPH")
  inputPath <- strOption
          ( long "input"
         <> metavar "PATH"
         <> help "The path to the input directory." )
  outputPath <- strOption
          ( long "output"
         <> metavar "PATH"
         <> help "The path to the output directory." )
  pure (graphPath, inputPath, outputPath)

inputsParser :: [Input] -> Parser ((FilePath, FilePath, FilePath) -> Results)
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
processNodes :: [Node] -> Catbox (Either [Text] ())
processNodes nodes = do
  results <- traverse processNode nodes
  case partitionEithers results of

    -- Nothing happened because all nodes have been processed; we're done!
    ([], []) ->
      pure (Right ())

    -- We still have nodes to process, but we were also unable to process any
    -- of them.
    (later, []) ->
      pure (Left ["Could not execute nodes: " <> T.intercalate ", " (nodeId <$> nodes)])

    -- We were able to process some nodes, so lets try the ones that failed
    -- again.
    (later, _) ->
      processNodes (fst <$> later)

processNode :: Node -> Catbox (Either (Node, [Text]) ())
processNode node = do
  results <- resolveParameters (nodeParameters node)
  case results of

    -- We failed to find all of the inputs required for this node.
    Left errs ->
      pure (Left (node, errs))

    -- We found all the required inputs, lets run the function now!
    Right args ->
      case (nodeFunction node, args) of

        ("parse_markdown", (CText a):[]) -> do
          results <- get
          let pandocResult =
                  runIdentity
                . flip evalStateT Pandoc.def
                . flip evalStateT Pandoc.def
                . runExceptT
                . Pandoc.unPandocPure
                $ Pandoc.readMarkdown Pandoc.def a
          case pandocResult of
            Left err -> pure (Left (node, [Pandoc.renderError err]))
            Right pandoc -> do
              put $
                Map.insert
                  (Key (nodeId node <> ".result"))
                  (CPandoc pandoc)
                  results
              pure (Right ())
        ("parse_markdown", arr) ->
          pure (Left (node, ["Wrong number of arguments for parse_markdown: " <> T.pack (show (length arr))]))

        ("render_html5", (CPandoc pandoc):[]) -> do
          results <- get
          let pandocResult =
                  runIdentity
                . flip evalStateT Pandoc.def
                . flip evalStateT Pandoc.def
                . runExceptT
                . Pandoc.unPandocPure
                $ Pandoc.writeHtml5 Pandoc.def pandoc
          case pandocResult of
            Left err -> pure (Left (node, [Pandoc.renderError err]))
            Right html -> do
              put $
                Map.insert
                  (Key (nodeId node <> ".result"))
                  (CText (TL.toStrict (Blaze.renderHtml html)))
                  results
              pure (Right ())
        ("render_html5", arr) ->
          pure (Left (node, ["Wrong number of arguments for render_html5: " <> T.pack (show (length arr))]))

        ("uppercase", (CText a):[]) -> do
          results <- get
          put $
            Map.insert
              (Key (nodeId node <> ".result"))
              (CText (T.toUpper a))
              results
          pure (Right ())
        ("uppercase", arr) ->
          pure (Left (node, ["Wrong number of arguments for uppercase: " <> T.pack (show (length arr))]))

        ("lowercase", (CText a):[]) -> do
          results <- get
          put $
            Map.insert
              (Key (nodeId node <> ".result"))
              (CText (T.toLower a))
              results
          pure (Right ())
        ("lowercase", arr) ->
          pure (Left (node, ["Wrong number of arguments for lowercase: " <> T.pack (show (length arr))]))

        ("concat", (CText a):(CText b):[]) -> do
          results <- get
          put $
            Map.insert
              (Key (nodeId node <> ".result"))
              (CText (a <> b))
              results
          pure (Right ())
        ("concat", arr) ->
          pure (Left (node, ["Wrong number of arguments for concat: " <> T.pack (show (length arr))]))

        ("read_file", (CFile file):[]) -> do
          values <- get
          put
            . Map.insert
                (Key (nodeId node <> ".text"))
                (CText (fileText file))
            . Map.insert
                (Key (nodeId node <> ".path"))
                (CFilePath (filePath file))
            $ values
          pure (Right ())
        ("read_file", arr) ->
          pure (Left (node, ["Wrong number of arguments for read_file: " <> T.pack (show (length arr))]))

        ("make_file", (CFilePath path):(CText text):[]) -> do
          results <- get
          put $
            Map.insert
              (Key (nodeId node <> ".result"))
              (CFile (File path text))
              results
          pure (Right ())
        ("make_file", arr) ->
          pure (Left (node, ["Wrong number of arguments for make_file: " <> T.pack (show (length arr))]))

        ("change_extension", (CText extension):(CFilePath path):[]) -> do
          results <- get
          put $
            Map.insert
              (Key (nodeId node <> ".result"))
              (CFilePath (FilePath.replaceExtension path (T.unpack extension)))
              results
          pure (Right ())
        ("change_extension", arr) ->
          pure (Left (node, ["Wrong number of arguments for change_extension: " <> T.pack (show (length arr))]))

        (function, _) ->
          pure (Left (node, ["Cannot find function " <> function]))
