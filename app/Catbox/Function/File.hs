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
    , functionInputs =
        [ Signature "path" TPath
        ]
    , functionOutputs =
        [ Signature "result" TText
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
    , functionInputs =
        [ Signature "path" TPath
        , Signature "text" TText
        ]
    , functionOutputs =
        [ Signature "result" TFile
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
