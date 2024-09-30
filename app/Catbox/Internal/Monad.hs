module Catbox.Internal.Monad
( Catbox
, evalCatbox
, runCatbox
, mapError
, throwError
, tryError

-- Helper functions
, insertKey
, resolveParameters
, resolveParameter
) where

import Catbox.Internal.Types
import Control.Monad.Except (MonadError, mapError, throwError, tryError)
import qualified Data.Map as Map

newtype Catbox e a = Catbox (ExceptT e (StateT Results Identity) a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    )
  deriving
    ( MonadState Results
    , MonadError e
    )

evalCatbox :: Catbox e a -> Results -> Either e a
evalCatbox (Catbox catbox) results =
  runIdentity . flip evalStateT results $ runExceptT catbox

runCatbox :: Catbox e a -> Results -> (Either e a, Results)
runCatbox (Catbox catbox) results =
  runIdentity . flip runStateT results $ runExceptT catbox

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

insertKey :: Key -> Value -> Catbox e ()
insertKey key value = do
  results <- get
  put (Map.insert key value results)

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
      results <- get
      case Map.lookup key results of
        Nothing ->
          throwError ("Cannot find " <> keyToText key)
        Just value ->
          pure (parameterName parameter, value)

