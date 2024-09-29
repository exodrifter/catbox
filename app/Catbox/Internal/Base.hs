module Catbox.Internal.Base
( baseFunctions
) where

import Catbox.Internal.Function
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

baseFunctions :: Map Text Function
baseFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ changeExtensionFunction
    , concatFunction
    , lowercaseFunction
    , makeFileFunction
    , readFileFunction
    , uppercaseFunction
    ]

changeExtensionFunction :: Function
changeExtensionFunction =
  Function { functionName = "change_extension", .. }
  where
    functionExec params key =
      let
        parse =
          (,)
            <$> getText "extension" params
            <*> getFilePath "path" params
      in
        case parse of
          Left err -> pure (Left err)
          Right (extension, path) -> do
            insertKey
              (key <> ".result")
              (CFilePath (FilePath.replaceExtension path (T.unpack extension)))
            pure (Right ())

concatFunction :: Function
concatFunction =
  Function { functionName = "concat", .. }
  where
    functionExec params key =
      let
        parse =
          (,)
            <$> getText "a" params
            <*> getText "b" params
      in
        case parse of
          Left err -> pure (Left err)
          Right (a, b) -> do
            insertKey
              (key <> ".result")
              (CText (a <> b))
            pure (Right ())

lowercaseFunction :: Function
lowercaseFunction =
  Function { functionName = "lowercase", .. }
  where
    functionExec params key =
      case getText "text" params of
        Left err -> pure (Left err)
        Right text -> do
          insertKey
            (key <> ".result")
            (CText (T.toLower text))
          pure (Right ())

makeFileFunction :: Function
makeFileFunction =
  Function { functionName = "make_file", .. }
  where
    functionExec params key =
      let
        parse =
          (,)
            <$> getFilePath "path" params
            <*> getText "text" params
      in
        case parse of
          Left err -> pure (Left err)
          Right (path, text) -> do
            insertKey
              (key <> ".result")
              (CFile (File path text))
            pure (Right ())

readFileFunction :: Function
readFileFunction =
  Function { functionName = "read_file", .. }
  where
    functionExec params key =
      case getFile "file" params of
        Left err -> pure (Left err)
        Right file -> do
          insertKey
            (key <> ".text")
            (CText (fileText file))
          insertKey
            (key <> ".path")
            (CFilePath (filePath file))
          pure (Right ())

uppercaseFunction :: Function
uppercaseFunction =
  Function { functionName = "uppercase", .. }
  where
    functionExec params key =
      case getText "text" params of
        Left err -> pure (Left err)
        Right text -> do
          insertKey
            (key <> ".result")
            (CText (T.toUpper text))
          pure (Right ())
