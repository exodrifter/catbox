module Catbox.Internal.Execute
( processGraph
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T

processGraph :: Graph -> CatboxState -> Either Text (Map Text Value)
processGraph graph initialState = do
  let
    catbox = processNodes (graphNodes graph)
    importedState =
      foldl'
        ( \s (key, value) ->
            s { catboxResults =
                  Map.insert
                    ("import." <> key)
                    (CGraph value)
                    (catboxResults s)
              }
        )
        initialState
        (Map.toList (graphImports graph))

  case runCatbox catbox importedState of
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

    (_, e):_
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
    (nodeType node)
    args
    (Key (nodeId node))

invoke :: NodeType -> Map Text Value -> Key -> Catbox Text ()
invoke nodeType params key =
  case nodeType of
    NodeFunction name -> do
      fn <- getFunction name
      functionExec fn params key

    NodeGraph importKey -> do
      graph <- getImport importKey

      -- Set the initial state of the graph.
      initialState <- get
      let
        toInput (name, v) = (Key ("in." <> name), v)
        sandboxState = initialState
          { catboxResults = Map.fromList (toInput <$> Map.toList params)
          }

      case processGraph graph sandboxState of
        Left err ->
          throwError err
        Right finalState -> do
          let
            loadResults (name, v) =
              insertKey (key <> "." <> Key name) v
          traverse_ loadResults (Map.toList finalState)
