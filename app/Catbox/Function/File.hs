module Catbox.Function.File
( fileFunctions
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map

fileFunctions :: Map Text Function
fileFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ fileContentsFunction
    , makeFileFunction
    ]

fileContentsFunction :: Function
fileContentsFunction =
  Function
    { functionName = "file_contents"
    , functionInputs = Map.fromList
        [ ("path", TPath)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TText)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      path <- pathParam "path" params
      contents <- getFileContents path
      insertKey
        (keyFromText key <> "result")
        (CText contents)

makeFileFunction :: Function
makeFileFunction =
  Function
    { functionName = "make_file"
    , functionInputs = Map.fromList
        [ ("path", TPath)
        , ("text", TText)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TFile)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      path <- pathParam "path" params
      text <- textParam "text" params
      insertKey
        (keyFromText key <> "result")
        (CFile (File path text))
