module Catbox.Function.Directory
( directoryFunctions
) where

import Catbox.Internal.Function
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map

directoryFunctions :: Map Text Function
directoryFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ listFilesFunction
    ]

listFilesFunction :: Function
listFilesFunction =
  Function { functionName = "list_files", .. }
  where
    functionExec params key = do
      path <- getFilePath "path" params
      files <- getFilePaths path
      insertKey
        (key <> ".result")
        (CArray (CFilePath <$> files))
