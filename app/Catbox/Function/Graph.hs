module Catbox.Function.Graph
( graphFunctions
) where

import Catbox.Internal.Execute
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T

graphFunctions :: Map Text Function
graphFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ execGraphFunction
    , mapFunction
    ]

execGraphFunction :: Function
execGraphFunction =
  Function { functionName = "exec_graph", .. }
  where
    functionExec params key = do
      graph <- graphParam "_graph" params

      -- Set the initial state of the graph.
      initialState <- get
      let
        sandboxState = initialState
          { catboxResults = paramsToGraphInputs params
          }

      case processGraph graph sandboxState of
        Left err ->
          throwError err
        Right finalState -> do
          loadResults key finalState

mapFunction :: Function
mapFunction =
  Function { functionName = "map", .. }
  where
    functionExec params key = do
      graph <- graphParam "_graph" params
      inputName <- Key . ("in." <> ) <$> textParam "_input" params
      list <- listParam "_list" params

      let
        graphInputs = paramsToGraphInputs params

      initialState <- get
      results <- traverse
        ( \a ->
            getResults
              graph
              initialState { catboxResults = Map.insert inputName a graphInputs }
        )
        list

      loadResults key (collectResults results)

    getResults :: Graph -> CatboxState -> Catbox Text (Map Text Value)
    getResults graph s =
      case processGraph graph s of
        Left err ->
          throwError err
        Right finalState -> do
          pure finalState

    collectResults :: [Map Text Value] -> Map Text Value
    collectResults results =
      let
        combine :: Map Text [Value] -> [(Text, Value)] -> Map Text [Value]
        combine =
          foldl'
            ( \a (key, value) ->
                if Map.member key a
                then Map.adjust (value:) key a
                else Map.insert key [value] a
            )
      in
        CList <$> foldl' combine Map.empty (Map.toList <$> results)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Use node inputs that don't start with `_` as inputs to the graph
paramsToGraphInputs :: Map Text Value -> Map Key Value
paramsToGraphInputs params =
  let
    toInput (name, v) =
      if "_" `T.isPrefixOf` name
      then Nothing
      else Just (Key ("in." <> name), v)
  in
    Map.fromList (mapMaybe toInput (Map.toList params))

loadResults :: Key -> Map Text Value -> Catbox e ()
loadResults key results = do
  let
    loadResult (name, v) =
      insertKey (key <> "." <> Key name) v
  traverse_ loadResult (Map.toList results)
