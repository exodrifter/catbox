module Catbox.Function.Path
( pathFunctions
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

pathFunctions :: Map Text Function
pathFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ changeExtensionFunction
    ]

changeExtensionFunction :: Function
changeExtensionFunction =
  Function
    { functionName = "change_extension"
    , functionInputs = Map.fromList
        [ ("extension", TText)
        , ("path", TPath)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TPath)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      extension <- textParam "extension" params
      path <- pathParam "path" params
      insertKey
        (key <> "result")
        (CPath (FilePath.replaceExtension path (T.unpack extension)))
