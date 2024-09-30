module Catbox.Function.Base
( baseFunctions
) where

import Catbox.Internal.Function
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

baseFunctions :: Map Text Function
baseFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ concatFunction
    , fileContentsFunction
    , lowercaseFunction
    , makeFileFunction
    , uppercaseFunction
    ]

concatFunction :: Function
concatFunction =
  Function { functionName = "concat", .. }
  where
    functionExec params key = do
      a <- getText "a" params
      b <- getText "b" params
      insertKey
        (key <> ".result")
        (CText (a <> b))

fileContentsFunction :: Function
fileContentsFunction =
  Function { functionName = "file_contents", .. }
  where
    functionExec params key = do
      path <- getFilePath "path" params
      contents <- getFileContents path
      insertKey
        (key <> ".result")
        (CText contents)

lowercaseFunction :: Function
lowercaseFunction =
  Function { functionName = "lowercase", .. }
  where
    functionExec params key = do
      text <- getText "text" params
      insertKey
        (key <> ".result")
        (CText (T.toLower text))

makeFileFunction :: Function
makeFileFunction =
  Function { functionName = "make_file", .. }
  where
    functionExec params key = do
      path <- getFilePath "path" params
      text <- getText "text" params
      insertKey
        (key <> ".result")
        (CFile (File path text))

uppercaseFunction :: Function
uppercaseFunction =
  Function { functionName = "uppercase", .. }
  where
    functionExec params key = do
      text <- getText "text" params
      insertKey
        (key <> ".result")
        (CText (T.toUpper text))
