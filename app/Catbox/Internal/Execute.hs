module Catbox.Internal.Execute
( processGraph
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

processGraph :: FilePath -> Graph -> CatboxState -> Either Text (Map Text Value)
processGraph path graph initialState = do
  let catbox = processNodes path (graphNodes graph)
  case runCatbox catbox initialState of
    (Left errs, _) -> Left errs
    (Right (), finalState) ->
      -- Filter the final state for the outputs
      extractResults finalState (graphOutputs graph)

-- Write results to disk and buffer only if all outputs are available.
extractResults :: CatboxState -> [Output] -> Either Text (Map Text Value)
extractResults s outputs = do
  let
    loadResult :: Output -> Either Text (Text, Value)
    loadResult output =
      (\(_, v) -> (outputName output, v)) <$>
        evalCatbox (resolveParameter (outputParameter output)) s

  case partitionEithers (loadResult <$> outputs) of
    -- All of the results are available
    ([], successfulReturns) -> Right (Map.fromList successfulReturns)

    -- Some results are missing!
    (failedOutputs, _) -> Left (T.intercalate "\n" failedOutputs)

-- Tries to process the output of all nodes in the graph. Returns an error if
-- it fails to do so.
processNodes :: FilePath -> [Node] -> Catbox Text ()
processNodes path nodes = do
  let
    tryExec :: [(Node, Text)] -> Node -> Catbox Text [(Node, Text)]
    tryExec failed node = do
      result <- tryError (processNode path node)
      case result of
        Left err -> pure ((node, err):failed)
        Right () -> pure failed

  failed <- foldlM tryExec [] nodes

  case failed of
    -- Finished processing all nodes
    [] -> pure ()

    (_, e):_
      -- We were unable to process any of the nodes.
      | length failed == length nodes ->
        throwError e -- TODO: Return all of the errors

      -- Some succeeded, try again
      | otherwise ->
        processNodes path (fst <$> failed)

processNode :: FilePath -> Node -> Catbox Text ()
processNode path node = do
  args <- resolveParameters (nodeParameters node)
  invoke
    path
    (nodeType node)
    args
    (Key (nodeId node))

invoke :: FilePath -> NodeType -> Map Text Value -> Key -> Catbox Text ()
invoke path nodeType params key =
  case nodeType of
    NodeFunction name -> do
      fn <- getFunction name
      functionExec fn params key

    NodeGraph graphPath -> do
      -- The path is relative to the graph we are currently invoking, so we have
      -- to rewrite the path such that it is relative to our working directory.
      let newPath = FilePath.combine (FilePath.takeDirectory path) graphPath
      graph <- getGraph newPath
      initialState <- get

      -- Set the input arguments to the graph
      let
        toInput (name, v) = (Key ("in." <> name), v)
        sandboxState = initialState
          { catboxResults = Map.fromList (toInput <$> Map.toList params) }

      case processGraph newPath graph sandboxState of
        Left err ->
          throwError err
        Right finalState -> do
          let
            loadResults (name, v) =
              insertKey (key <> "." <> Key name) v
          traverse_ loadResults (Map.toList finalState)
