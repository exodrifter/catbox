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
  Function
    { functionName = "exec_graph"
    , functionInputs = Map.fromList
        [ ("_graph", TGraph)
        ]
    , functionOutputs = Map.empty
    , functionVariableInputs = True
    , functionVariableOutputs = True
    , ..
    }
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
          let
            loadResult (name, v) =
              insertKey (keyFromText key <> keyFromText name) v
          traverse_ loadResult (Map.toList finalState)

mapFunction :: Function
mapFunction =
  Function
    { functionName = "map"
    , functionInputs = Map.fromList
        [ ("_graph", TGraph)
        , ("_input", TText)
        , ("_list", TList)
        ]
    , functionOutputs = Map.empty
    , functionVariableInputs = True
    , functionVariableOutputs = True
    , ..
    }
  where
    functionExec params key = do
      graph <- graphParam "_graph" params
      inputName <- ("in" <>) . keyFromText <$> textParam "_input" params
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

      insertKey (keyFromText key <> "result") (CList (catMaybes results))

    getResults :: Graph -> CatboxState -> Catbox Text (Maybe Value)
    getResults graph s =
      case processGraph graph s of
        Left err ->
          throwError err
        Right finalState -> do
          case Map.toList finalState of
            [] -> pure Nothing
            [(_, v)] -> pure (Just v)
            _ -> pure (Just (CObject (Object finalState)))

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
      else Just ("in" <> keyFromText name, v)
  in
    Map.fromList (mapMaybe toInput (Map.toList params))
