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
    catbox = processNodes graph (graphNodes graph)
    importedState =
      foldl'
        ( \s (name, value) ->
            s { catboxResults =
                  Map.insert
                    ("import" <> keyFromText name)
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
      extractResults finalState (graphOutputs graph) (graphParameters graph)

-- Write results to disk and buffer only if all outputs are available.
extractResults :: CatboxState -> [Signature] -> [Parameter] -> Either Text (Map Text Value)
extractResults s outputs params = do
  let
    loadResult :: Signature -> Either Text (Text, Value)
    loadResult output = do
      let
        outputKey = "out" <> keyFromText (signatureName output)
      param <-
        case filter (\p -> parameterKey p == outputKey) params of
          [p] -> Right p
          [] -> Left ("Cannot find parameter \"" <> keyToText outputKey <> "\"")
          _ -> Left ("Multiple parameters for \"" <> keyToText outputKey <> "\"")
      (_, v) <- evalCatbox (resolveParameter param) s
      pure (signatureName output, v)

  case partitionEithers (loadResult <$> outputs) of
    -- All of the results are available
    ([], successfulReturns) -> Right (Map.fromList successfulReturns)

    -- Some results are missing!
    (failedOutputs, _) -> Left (T.intercalate "\n" failedOutputs)

-- Tries to process the output of all nodes in the graph. Returns an error if
-- it fails to do so.
processNodes :: Graph -> [Node] -> Catbox Text ()
processNodes graph nodes = do
  let
    tryExec :: [(Node, Text)] -> Node -> Catbox Text [(Node, Text)]
    tryExec failed node = do
      result <- tryError (processNode graph node)
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
        processNodes graph (fst <$> failed)

processNode :: Graph -> Node -> Catbox Text ()
processNode graph node = do
  args <- resolveParameters (nodeId node) (graphParameters graph)
  invoke (nodeFunction node) args (nodeId node)

invoke :: Text -> Map Text Value -> Text -> Catbox Text ()
invoke name params key = do
  fn <- getFunction name
  functionExec fn params key
