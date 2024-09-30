module Catbox.Function.Pandoc
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

runPandocPure :: Pandoc.PandocPure a -> Catbox Text a
runPandocPure pandoc =
  let
    result =
        runIdentity
      . flip evalStateT Pandoc.def
      . flip evalStateT Pandoc.def
      . runExceptT
      $ Pandoc.unPandocPure pandoc
  in
    case result of
      Left err -> throwError (Pandoc.renderError err)
      Right a -> pure a

parseMarkdownFunction :: Function
parseMarkdownFunction =
  Function { functionName = "parse_markdown", .. }
  where
    functionExec params key = do
      text <- getText "text" params
      pandoc <- runPandocPure (Pandoc.readMarkdown Pandoc.def text)
      insertKey (key <> ".result") (CPandoc pandoc)

renderHtml5Function :: Function
renderHtml5Function =
  Function { functionName = "render_html5", .. }
  where
    functionExec params key = do
      pandoc <- getPandoc "pandoc" params
      html <- runPandocPure (Pandoc.writeHtml5 Pandoc.def pandoc)
      let text = TL.toStrict (Blaze.renderHtml html)
      insertKey (key <> ".result") (CText text)
