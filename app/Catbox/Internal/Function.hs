module Catbox.Internal.Function
( Function(..)
, fileParam
, pathParam
, pandocParam
, textParam
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types

import Text.Pandoc (Pandoc)
import qualified Data.Map as Map

data Function =
  Function
    { functionName :: Text
    , functionExec :: Map Text Value -> Key -> Catbox Text ()
    }

-------------------------------------------------------------------------------
-- function helpers
-------------------------------------------------------------------------------

fileParam :: Text -> Map Text Value -> Catbox Text File
fileParam name params =
  case Map.lookup name params of
    Just (CFile v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a file")
    _ -> throwError ("Cannot find parameter " <> name)

pathParam :: Text -> Map Text Value -> Catbox Text FilePath
pathParam name params =
  case Map.lookup name params of
    Just (CPath v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a path")
    _ -> throwError ("Cannot find parameter " <> name)

pandocParam :: Text -> Map Text Value -> Catbox Text Pandoc
pandocParam name params =
  case Map.lookup name params of
    Just (CPandoc v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a pandoc")
    _ -> throwError ("Cannot find parameter " <> name)

textParam :: Text -> Map Text Value -> Catbox Text Text
textParam name params =
  case Map.lookup name params of
    Just (CText v) -> pure v
    Just _ -> throwError ("Parameter \"" <> name <> "\" is not a text")
    _ -> throwError ("Cannot find parameter " <> name)
