module Catbox.Function.Base
( baseFunctions
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T

baseFunctions :: Map Text Function
baseFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ concatFunction
    , deconstructFunction
    , lowercaseFunction
    , uppercaseFunction
    ]

concatFunction :: Function
concatFunction =
  Function
    { functionName = "concat"
    , functionParams = Map.fromList
        [ ("a", TText)
        , ("b", TText)
        ]
    , functionEnableVariableParams = False
    , ..
    }
  where
    functionExec params key = do
      a <- textParam "a" params
      b <- textParam "b" params
      insertKey
        (key <> "result")
        (CText (a <> b))

deconstructFunction :: Function
deconstructFunction =
  Function
    { functionName = "deconstruct"
    , functionParams = Map.fromList
      [ ("object", TObject)
      ]
    , functionEnableVariableParams = False
    , ..
    }
  where
    functionExec params key = do
      object <- objectParam "object" params
      let writeKeys (k, v) = insertKey (key <> keyFromText k) v
      traverse_ writeKeys (Map.toList (objectFields object))

lowercaseFunction :: Function
lowercaseFunction =
  Function
    { functionName = "lowercase"
    , functionParams = Map.fromList
        [ ("text", TText)
        ]
    , functionEnableVariableParams = False
    , ..
    }
  where
    functionExec params key = do
      text <- textParam "text" params
      insertKey
        (key <> "result")
        (CText (T.toLower text))

uppercaseFunction :: Function
uppercaseFunction =
  Function
    { functionName = "uppercase"
    , functionParams = Map.fromList
        [ ("text", TText)
        ]
    , functionEnableVariableParams = False
    , ..
    }
  where
    functionExec params key = do
      text <- textParam "text" params
      insertKey
        (key <> "result")
        (CText (T.toUpper text))
