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
    , functionExec :: Map Text Value -> Key -> Catbox Text ()
    }

-------------------------------------------------------------------------------
-- function helpers
-------------------------------------------------------------------------------

invoke :: Map Text Function -> NodeType -> Map Text Value -> Key -> Catbox Text ()
invoke functions nodeType params key =
  case nodeType of
    NodeFunction name ->
      case Map.lookup name functions of
        Nothing -> throwError ("Cannot find function \"" <> name <> "\"")
        Just fn -> functionExec fn params key

    NodeGraph _path ->
      throwError "Sub graphs are not implemented yet!"

getFile :: Text -> Map Text Value -> Catbox Text File
getFile name params =
  case Map.lookup name params of
    Just (CFile v) -> pure v
    Just _ -> throwError ("Cannot convert parameter " <> name <> " to file")
    _ -> throwError ("Cannot find parameter " <> name)

getFilePath :: Text -> Map Text Value -> Catbox Text FilePath
getFilePath name params =
  case Map.lookup name params of
    Just (CFilePath v) -> pure v
    Just _ -> throwError ("Cannot convert parameter " <> name <> " to file path")
    _ -> throwError ("Cannot find parameter " <> name)

getPandoc :: Text -> Map Text Value -> Catbox Text Pandoc
getPandoc name params =
  case Map.lookup name params of
    Just (CPandoc v) -> pure v
    Just _ -> throwError ("Cannot convert parameter " <> name <> " to pandoc")
    _ -> throwError ("Cannot find parameter " <> name)

getText :: Text -> Map Text Value -> Catbox Text Text
getText name params =
  case Map.lookup name params of
    Just (CText v) -> pure v
    Just _ -> throwError ("Cannot convert parameter " <> name <> " to text")
    _ -> throwError ("Cannot find parameter " <> name)
