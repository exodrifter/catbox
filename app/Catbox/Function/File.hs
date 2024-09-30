module Catbox.Function.File
( fileFunctions
) where

import Catbox.Internal.Function
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

fileFunctions :: Map Text Function
fileFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ fileContentsFunction
    , makeFileFunction
    ]

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
