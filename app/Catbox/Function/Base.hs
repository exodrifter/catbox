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
    , lowercaseFunction
    , uppercaseFunction
    ]

concatFunction :: Function
concatFunction =
  Function { functionName = "concat", .. }
  where
    functionExec params key = do
      a <- textParam "a" params
      b <- textParam "b" params
      insertKey
        (key <> "result")
        (CText (a <> b))

lowercaseFunction :: Function
lowercaseFunction =
  Function { functionName = "lowercase", .. }
  where
    functionExec params key = do
      text <- textParam "text" params
      insertKey
        (key <> "result")
        (CText (T.toLower text))

uppercaseFunction :: Function
uppercaseFunction =
  Function { functionName = "uppercase", .. }
  where
    functionExec params key = do
      text <- textParam "text" params
      insertKey
        (key <> "result")
        (CText (T.toUpper text))
