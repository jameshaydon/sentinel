-- | Pretty-printing with semantic annotations.
--
-- This module provides a 'Disp' typeclass for displaying values, along with
-- semantic annotation types that can be converted to ANSI terminal styles.
module Sentinel.Pretty
  ( -- * Display typeclass
    Disp (..),
    dispText,

    -- * Semantic annotations
    Ann (..),
    toAnsiStyle,

    -- * Rendering
    renderDoc,
    renderDocPlain,
    putDocLn,

    -- * Style helpers
    styled,
    header,
    subheader,
    label,
    styledToolName,
    thinking,
    styledObservation,
    errorText,
    successText,
    iterationNum,
    wrappedText,
    dimText,

    -- * Layout helpers
    section,
    sectionNoBody,
    toolDisplay,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as Terminal
import Prettyprinter.Render.Text qualified as TextRender
import Prelude

-- | Semantic annotation type for pretty-printing.
--
-- Instead of using raw ANSI styles directly, we use semantic annotations
-- that describe /what/ something is, not /how/ it should look. This allows
-- for consistent theming and easier maintenance.
data Ann
  = -- | Main section headers (e.g., "# Iteration 1")
    AnnHeader
  | -- | Subsection headers (e.g., "## Thinking")
    AnnSubheader
  | -- | Labels for key-value pairs (e.g., "Tool:", "Input:")
    AnnLabel
  | -- | Tool names
    AnnToolName
  | -- | Agent's thinking/reasoning text
    AnnThinking
  | -- | Tool observation/result text
    AnnObservation
  | -- | Final answer text
    AnnFinalAnswer
  | -- | Error messages
    AnnError
  | -- | User input/queries
    AnnUserText
  | -- | Iteration numbers
    AnnIterationNum
  | -- | Dimmed/secondary text
    AnnDim
  | -- | Success indicators
    AnnSuccess
  deriving stock (Show, Eq)

-- | Convert semantic annotations to ANSI terminal styles.
toAnsiStyle :: Ann -> AnsiStyle
toAnsiStyle = \case
  AnnHeader -> Terminal.bold <> Terminal.colorDull Terminal.Blue
  AnnSubheader -> Terminal.bold <> Terminal.colorDull Terminal.Magenta
  AnnLabel -> Terminal.bold
  AnnToolName -> Terminal.bold <> Terminal.colorDull Terminal.Green
  AnnThinking -> Terminal.italicized
  AnnObservation -> Terminal.colorDull Terminal.White
  AnnFinalAnswer -> mempty
  AnnError -> Terminal.bold <> Terminal.color Terminal.Red
  AnnUserText -> Terminal.colorDull Terminal.Blue
  AnnIterationNum -> Terminal.bold <> Terminal.colorDull Terminal.Magenta
  AnnDim -> Terminal.colorDull Terminal.White
  AnnSuccess -> Terminal.color Terminal.Green

-- | Layout options with a maximum line width of 120 characters.
layoutOpts :: LayoutOptions
layoutOpts = LayoutOptions (AvailablePerLine 120 1.0)

-- | Render a Doc to Text with ANSI styling.
renderDoc :: Doc Ann -> Text
renderDoc = Terminal.renderStrict . reAnnotateS toAnsiStyle . layoutPretty layoutOpts

-- | Render a Doc to plain Text without any styling.
renderDocPlain :: Doc ann -> Text
renderDocPlain = TextRender.renderStrict . layoutPretty layoutOpts

-- | Print a Doc to stdout, followed by a newline.
putDocLn :: Doc Ann -> IO ()
putDocLn doc = putStrLn (Text.unpack (renderDoc doc))

-- | A class for things that have a canonical pretty-printed representation.
class Disp a where
  disp :: a -> Doc Ann

-- | Render a displayable value to plain text (no ANSI styling).
dispText :: (Disp a) => a -> Text
dispText = renderDocPlain . disp

--------------------------------------------------------------------------------
-- Style helpers
--------------------------------------------------------------------------------

-- | Apply a semantic style to a document.
styled :: Ann -> Doc Ann -> Doc Ann
styled = annotate

-- | Style text as a main header.
header :: Doc Ann -> Doc Ann
header = styled AnnHeader

-- | Style text as a subheader.
subheader :: Doc Ann -> Doc Ann
subheader = styled AnnSubheader

-- | Style text as a label.
label :: Doc Ann -> Doc Ann
label = styled AnnLabel

-- | Style text as a tool name.
styledToolName :: Doc Ann -> Doc Ann
styledToolName = styled AnnToolName

-- | Style text as thinking/reasoning.
thinking :: Doc Ann -> Doc Ann
thinking = styled AnnThinking

-- | Style text as an observation/result.
styledObservation :: Doc Ann -> Doc Ann
styledObservation = styled AnnObservation

-- | Style text as an error.
errorText :: Doc Ann -> Doc Ann
errorText = styled AnnError

-- | Style text as a success indicator.
successText :: Doc Ann -> Doc Ann
successText = styled AnnSuccess

-- | Style text as an iteration number.
iterationNum :: Doc Ann -> Doc Ann
iterationNum = styled AnnIterationNum

-- | Style text as dimmed/secondary.
dimText :: Doc Ann -> Doc Ann
dimText = styled AnnDim

-- | Wrap text respecting line breaks and allowing proper text reflow.
-- Uses fillSep to wrap words within each line, preserving explicit line breaks.
wrappedText :: Text -> Doc Ann
wrappedText txt =
  let textLines = Text.lines txt
      wrapLine ln = fillSep (pretty <$> Text.words ln)
   in vsep (wrapLine <$> textLines)

--------------------------------------------------------------------------------
-- Layout helpers
--------------------------------------------------------------------------------

-- | Create a section with a header and body content.
-- This abstracts the common pattern: @nest n $ vsep ["", headerDoc, "", body]@
section :: Int -> Doc Ann -> Doc Ann -> Doc Ann
section indentLevel headerDoc body =
  nest indentLevel $ vsep ["", headerDoc, "", body]

-- | Create a section with just a header (no body content).
-- This abstracts the common pattern: @nest n $ vsep ["", headerDoc]@
sectionNoBody :: Int -> Doc Ann -> Doc Ann
sectionNoBody indentLevel headerDoc =
  nest indentLevel $ vsep ["", headerDoc]

-- | Display tool information with name and input.
-- Used for both LLM-initiated and Sentinel-initiated tool calls.
toolDisplay :: Doc Ann -> Text -> Text -> Doc Ann
toolDisplay headerDoc toolName input =
  vsep
    [ headerDoc,
      "",
      label "Tool:" <+> styledToolName (pretty toolName),
      label "Input:" <+> dimText (wrappedText input)
    ]
