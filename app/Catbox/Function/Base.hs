module Catbox.Function.Base
( baseFunctions
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map

baseFunctions :: Map Text Function
baseFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ deconstructFunction
    ]

deconstructFunction :: Function
deconstructFunction =
  Function
    { functionName = "deconstruct"
    , functionInputs = Map.fromList
        [ ("object", TObject)
        ]
    , functionOutputs = Map.empty
    , functionVariableInputs = False
    , functionVariableOutputs = True
    , ..
    }
  where
    functionExec params key = do
      object <- objectParam "object" params
      let writeKeys (k, v) = insertKey (keyFromText key <> keyFromText k) v
      traverse_ writeKeys (Map.toList (objectFields object))
