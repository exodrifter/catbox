module Catbox.Function.Pandoc
( pandocFunctions
) where

import Catbox.Internal.Monad
import Catbox.Internal.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Pandoc as Pandoc
import Text.Pandoc (Pandoc(..), Block (..), Inline (..))

pandocFunctions :: Map Text Function
pandocFunctions =
  Map.fromList $ (\g -> (functionName g, g)) <$>
    [ parseMarkdownFunction
    , renderHtml5Function

    -- Transformations
    , remapLinkExtension
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
  Function
    { functionName = "parse_markdown"
    , functionInputs = Map.fromList
        [ ("text", TText)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TPandoc)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      text <- textParam "text" params
      pandoc <- runPandocPure (Pandoc.readMarkdown Pandoc.def text)
      insertKey (keyFromText key <> "result") (CPandoc pandoc)

renderHtml5Function :: Function
renderHtml5Function =
  Function
    { functionName = "render_html5"
    , functionInputs = Map.fromList
        [ ("pandoc", TPandoc)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TText)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      pandoc <- pandocParam "pandoc" params
      html <- runPandocPure (Pandoc.writeHtml5 Pandoc.def pandoc)
      let text = TL.toStrict (Blaze.renderHtml html)
      insertKey (keyFromText key <> "result") (CText text)

--------------------------------------------------------------------------------
-- Transformation
--------------------------------------------------------------------------------

remapLinkExtension :: Function
remapLinkExtension =
  Function
    { functionName = "remap_link_extension"
    , functionInputs = Map.fromList
        [ ("pandoc", TPandoc)
        ]
    , functionOutputs = Map.fromList
        [ ("result", TPandoc)
        ]
    , functionVariableInputs = False
    , functionVariableOutputs = False
    , ..
    }
  where
    functionExec params key = do
      pandoc <- pandocParam "pandoc" params
      from <- textParam "from" params
      to <- textParam "to" params
      insertKey
        (keyFromText key <> "result")
        (CPandoc (remapTarget (first (T.replace from to)) pandoc))

remapTarget :: (Pandoc.Target -> Pandoc.Target) -> Pandoc -> Pandoc
remapTarget fn (Pandoc m b) =
  Pandoc m (remapBlocks <$> b)
  where
    remapBlocks block =
      case block of
        Plain inlines -> Plain (remapInline <$> inlines)
        Para inlines -> Para (remapInline <$> inlines)
        LineBlock inlines -> LineBlock (fmap remapInline <$> inlines)
        CodeBlock attr text -> CodeBlock attr text
        RawBlock format text -> RawBlock format text
        BlockQuote blocks -> BlockQuote (remapBlocks <$> blocks)
        OrderedList listAttributes blocks -> OrderedList listAttributes (fmap remapBlocks <$> blocks)
        BulletList blocks -> BulletList (fmap remapBlocks <$> blocks)
        DefinitionList definitions -> DefinitionList (remapDefinitions <$> definitions)
        Header int attr inlines -> Header int attr (remapInline <$> inlines)
        HorizontalRule -> HorizontalRule
        Table attr caption colSpec tablehead tableBody tableFoot -> Table attr caption colSpec tablehead tableBody tableFoot
        Figure attr caption blocks -> Figure attr caption (remapBlocks <$> blocks)
        Div attr blocks -> Div attr (remapBlocks <$> blocks)

    remapDefinitions (inlines, blocks) =
      (remapInline <$> inlines, fmap remapBlocks <$> blocks)

    remapInline inline =
      case inline of
        Str x -> Str x
        Emph inlines -> Emph (remapInline <$> inlines)
        Underline inlines -> Underline (remapInline <$> inlines)
        Strong inlines -> Strong (remapInline <$> inlines)
        Strikeout inlines -> Strikeout (remapInline <$> inlines)
        Superscript inlines -> Superscript (remapInline <$> inlines)
        Subscript inlines -> Subscript (remapInline <$> inlines)
        SmallCaps inlines -> SmallCaps (remapInline <$> inlines)
        Quoted quoteType inlines -> Quoted quoteType (remapInline <$> inlines)
        Cite citations inlines -> Cite citations (remapInline <$> inlines)
        Code attr text -> Code attr text
        Space -> Space
        SoftBreak -> SoftBreak
        LineBreak -> LineBreak
        Math mathType text -> Math mathType text
        RawInline format text -> RawInline format text
        Link attr inlines target -> Link attr (remapInline <$> inlines) (fn target)
        Image attr inlines target -> Image attr (remapInline <$> inlines) (fn target)
        Note blocks -> Note (remapBlocks <$> blocks)
        Span attr inlines -> Span attr (remapInline <$> inlines)
