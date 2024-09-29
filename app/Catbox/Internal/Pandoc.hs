module Catbox.Internal.Pandoc
( pandocFunctions
) where

import Catbox.Internal.Function
import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Pandoc as Pandoc

pandocFunctions :: Map Text Function
pandocFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ parseMarkdownFunction
    , renderHtml5Function
    ]

runPandocPure :: Pandoc.PandocPure a -> Either Pandoc.PandocError a
runPandocPure =
    runIdentity
  . flip evalStateT Pandoc.def
  . flip evalStateT Pandoc.def
  . runExceptT
  . Pandoc.unPandocPure

parseMarkdownFunction :: Function
parseMarkdownFunction =
  Function { functionName = "parse_markdown", .. }
  where
    functionExec params key =
      case getText "text" params of
        Left err -> pure (Left err)
        Right text -> do
          case runPandocPure (Pandoc.readMarkdown Pandoc.def text) of
            Left err -> pure (Left (Pandoc.renderError err))
            Right pandoc -> do
              insertKey
                (key <> ".result")
                (CPandoc pandoc)
              pure (Right ())

renderHtml5Function :: Function
renderHtml5Function =
  Function { functionName = "render_html5", .. }
  where
    functionExec params key =
      case getPandoc "pandoc" params of
        Left err -> pure (Left err)
        Right pandoc -> do
          case runPandocPure (Pandoc.writeHtml5 Pandoc.def pandoc) of
            Left err -> pure (Left (Pandoc.renderError err))
            Right html -> do
              insertKey
                (key <> ".result")
                (CText (TL.toStrict (Blaze.renderHtml html)))
              pure (Right ())
