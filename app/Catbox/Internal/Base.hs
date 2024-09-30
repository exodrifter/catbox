module Catbox.Internal.Base
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
    [ changeExtensionFunction
    , concatFunction
    , lowercaseFunction
    , makeFileFunction
    , readFileFunction
    , uppercaseFunction
    ]

changeExtensionFunction :: Function
changeExtensionFunction =
  Function { functionName = "change_extension", .. }
  where
    functionExec params key = do
      extension <- getText "extension" params
      path <- getFilePath "path" params
      insertKey
        (key <> ".result")
        (CFilePath (FilePath.replaceExtension path (T.unpack extension)))

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

readFileFunction :: Function
readFileFunction =
  Function { functionName = "read_file", .. }
  where
    functionExec params key = do
      file <- getFile "file" params
      insertKey
        (key <> ".text")
        (CText (fileText file))
      insertKey
        (key <> ".path")
        (CFilePath (filePath file))

uppercaseFunction :: Function
uppercaseFunction =
  Function { functionName = "uppercase", .. }
  where
    functionExec params key = do
      text <- getText "text" params
      insertKey
        (key <> ".result")
        (CText (T.toUpper text))
