module Catbox.Internal.Monad
( Catbox
, CatboxState(..)
, evalCatbox
, runCatbox
, mapError
, throwError
, tryError

-- File functions
, getFileContents

-- Result functions
, insertKey
, resolveParameters
, resolveParameter
) where

import Catbox.Internal.Types
import Control.Monad.Except (MonadError, mapError, throwError, tryError)
import qualified Data.Map as Map
import qualified Data.Text as T

newtype Catbox e a = Catbox (ExceptT e (StateT CatboxState Identity) a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    )
  deriving
    ( MonadState CatboxState
    , MonadError e
    )

data CatboxState =
  CatboxState
    -- Stores the result for each key in the node graph.
    { catboxResults :: Map Key Value
    , catboxFiles :: Map FilePath Text
    }

evalCatbox :: Catbox e a -> CatboxState -> Either e a
evalCatbox (Catbox catbox) results =
  runIdentity . flip evalStateT results $ runExceptT catbox

runCatbox :: Catbox e a -> CatboxState -> (Either e a, CatboxState)
runCatbox (Catbox catbox) results =
  runIdentity . flip runStateT results $ runExceptT catbox

-------------------------------------------------------------------------------
-- File Functions
-------------------------------------------------------------------------------

getFileContents :: FilePath -> Catbox Text Text
getFileContents path = do
  files <- gets catboxFiles
  case Map.lookup path files of
    Nothing -> throwError ("Cannot find file \"" <> T.pack path <> "\"")
    Just file -> pure file

-------------------------------------------------------------------------------
-- Result Functions
-------------------------------------------------------------------------------

insertKey :: Key -> Value -> Catbox e ()
insertKey key value = do
  state <- get
  put state { catboxResults = Map.insert key value (catboxResults state) }

resolveParameters :: [Parameter] -> Catbox Text (Map Text Value)
resolveParameters parameters = do
  results <- traverse resolveParameter parameters
  pure (Map.fromList results)

resolveParameter :: Parameter -> Catbox Text (Text, Value)
resolveParameter parameter = do
  case parameterSource parameter of
    Constant const ->
      pure (parameterName parameter, const)
    Connection key -> do
      results <- gets catboxResults
      case Map.lookup key results of
        Nothing ->
          throwError ("Cannot find " <> keyToText key)
        Just value ->
          pure (parameterName parameter, value)
