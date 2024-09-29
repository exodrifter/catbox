module Catbox.Internal.Monad
( Catbox
, evalCatbox
, runCatbox

-- Helper functions
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

resolveParameters :: [Parameter] -> Catbox (Either [Text] [Value])
resolveParameters parameters = do
  results <- traverse resolveParameter parameters
  pure (collectEithers results)

resolveParameter :: Parameter -> Catbox (Either Text Value)
resolveParameter parameter =
  case parameter of
    Constant value ->
      pure (Right value)
    Connection key -> do
      cache <- get
      case Map.lookup key cache of
        Nothing -> pure (Left ("Cannot find " <> keyToText key))
        Just arg -> pure (Right arg)

collectEithers :: [Either a b] -> Either [a] [b]
collectEithers eithers =
  case partitionEithers eithers of
    ([], results) ->
      Right results
    (errors, _) ->
      Left errors
