module Catbox.Internal.Monad
( Catbox
, runCatbox

-- Helper functions
, lookupResults
, lookupResult
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

runCatbox :: Catbox a -> Results -> (a, Results)
runCatbox (Catbox stateT) =
  runIdentity . runStateT stateT

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

lookupResults :: [Key] -> Catbox (Either [Text] [Value])
lookupResults connections = do
  results <- traverse lookupResult connections
  pure (collectEithers results)

lookupResult :: Key -> Catbox (Either Text Value)
lookupResult connection = do
  cache <- get
  case Map.lookup connection cache of
    Nothing -> pure (Left ("Cannot find " <> keyToText connection))
    Just arg -> pure (Right arg)

collectEithers :: [Either a b] -> Either [a] [b]
collectEithers eithers =
  case partitionEithers eithers of
    ([], results) ->
      Right results
    (errors, _) ->
      Left errors
