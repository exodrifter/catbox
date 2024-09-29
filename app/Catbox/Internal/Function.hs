{-# LANGUAGE RankNTypes #-}
module Catbox.Internal.Function
( Function(..)
, invoke
, getFile
, getFilePath
, getPandoc
, getText
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types

import Text.Pandoc (Pandoc)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Dynamic as Dynamic
import qualified Type.Reflection as Reflection

data Function =
  Function
    { functionName :: Text
    , functionExec :: Map Text Value -> Key -> Catbox (Either Text ())
    }

-------------------------------------------------------------------------------
-- function helpers
-------------------------------------------------------------------------------

invoke :: Map Text Function -> Text -> Map Text Value -> Key -> Catbox (Either Text ())
invoke functions name params key =
  case Map.lookup name functions of
    Nothing -> pure (Left ("Cannot find function \"" <> name <> "\""))
    Just fn -> functionExec fn params key

getFile :: Text -> Map Text Value -> Either Text File
getFile name params =
  case Map.lookup name params of
    Just (CFile v) -> Right v
    Just _ -> Left ("Cannot convert parameter " <> name <> " to file")
    _ -> Left ("Cannot find parameter " <> name)

getFilePath :: Text -> Map Text Value -> Either Text FilePath
getFilePath name params =
  case Map.lookup name params of
    Just (CFilePath v) -> Right v
    Just _ -> Left ("Cannot convert parameter " <> name <> " to file path")
    _ -> Left ("Cannot find parameter " <> name)

getPandoc :: Text -> Map Text Value -> Either Text Pandoc
getPandoc name params =
  case Map.lookup name params of
    Just (CPandoc v) -> Right v
    Just _ -> Left ("Cannot convert parameter " <> name <> " to pandoc")
    _ -> Left ("Cannot find parameter " <> name)

getText :: Text -> Map Text Value -> Either Text Text
getText name params =
  case Map.lookup name params of
    Just (CText v) -> Right v
    Just _ -> Left ("Cannot convert parameter " <> name <> " to text")
    _ -> Left ("Cannot find parameter " <> name)
