module Catbox.Internal.Monad
( Catbox
, evalCatbox
, runCatbox

-- Helper functions
, insertKey
, resolveParameters
, resolveParameter
) where

import Catbox.Internal.Types
import qualified Data.Map as Map

newtype Catbox a = Catbox (StateT Results Identity a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    )
  deriving
    ( MonadState Results
    )

evalCatbox :: Catbox a -> Results -> a
evalCatbox (Catbox stateT) =
  runIdentity . evalStateT stateT

runCatbox :: Catbox a -> Results -> (a, Results)
runCatbox (Catbox stateT) =
  runIdentity . runStateT stateT

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

insertKey :: Key -> Value -> Catbox ()
insertKey key value = do
  results <- get
  put (Map.insert key value results)

resolveParameters :: [Parameter] -> Catbox (Either [Text] (Map Text Value))
resolveParameters parameters = do
  results <- traverse resolveParameter parameters
  case partitionEithers results of
    ([], results) ->
      pure (Right (Map.fromList results))
    (errors, _) ->
      pure (Left errors)

resolveParameter :: Parameter -> Catbox (Either Text (Text, Value))
resolveParameter parameter = do
  case parameterSource parameter of
    Constant const ->
      pure (Right (parameterName parameter, const))
    Connection key -> do
      results <- get
      case Map.lookup key results of
        Nothing ->
          pure (Left ("Cannot find " <> keyToText key))
        Just value ->
          pure (Right (parameterName parameter, value))

