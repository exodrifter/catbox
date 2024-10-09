module Catbox.Function
( module X
, standardFunctions
) where

import Catbox.Function.Base as X
import Catbox.Function.File as X
import Catbox.Function.Graph as X
import Catbox.Function.Pandoc as X
import Catbox.Function.Path as X

import Catbox.Internal.Monad (Function)
import qualified Data.Map as Map

standardFunctions :: Map Text Function
standardFunctions =
  Map.unions
    [ baseFunctions
    , fileFunctions
    , graphFunctions
    , pandocFunctions
    , pathFunctions
    ]
