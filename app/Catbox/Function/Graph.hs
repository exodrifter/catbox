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
    ]

execGraphFunction :: Function
execGraphFunction =
  Function { functionName = "exec_graph", .. }
  where
    functionExec params key = do
      graph <- graphParam "_graph" params

      -- Use node inputs that don't start with `_` as inputs to the graph
      let
        toInput (name, v) =
          if "_" `T.isPrefixOf` name
          then Nothing
          else Just (Key ("in." <> name), v)
        catboxResults = Map.fromList (mapMaybe toInput (Map.toList params))

      -- Set the initial state of the graph.
      initialState <- get
      let
        sandboxState = initialState
          { catboxResults
          }

      case processGraph graph sandboxState of
        Left err ->
          throwError err
        Right finalState -> do
          let
            loadResults (name, v) =
              insertKey (key <> "." <> Key name) v
          traverse_ loadResults (Map.toList finalState)
