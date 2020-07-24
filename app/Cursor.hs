{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Cursor where

import Data.Text as Text
import Brick.Widgets.Core

data DocCursor =
  DocCursor
  { docBefore :: Text
  , docLineNo  :: Int
  , docCurr   :: LineCursor
  , docAfter  :: Text
  }
  deriving (Eq, Show)

docEmpty :: DocCursor
docEmpty =
  DocCursor mempty 0 lcEmpty mempty

docEdLine :: (LineCursor -> LineCursor) -> DocCursor -> DocCursor
docEdLine lcFn docCursor@DocCursor{..}
  = docCursor{docCurr = lcFn docCurr}

docNewLine :: DocCursor -> DocCursor
docNewLine DocCursor{..} = DocCursor before (docLineNo + 1) lcEmpty docAfter
  where
    before = (docBefore <> lcToText docCurr) `snoc` '\n'

-- to do: doc delete dealing with start of line and with first line

docToText :: DocCursor -> Text
docToText DocCursor{..} =
  docBefore <> (lcToText docCurr) <> docAfter

data LineCursor =
  LineCursor
  { lineBefore :: Text
  , lineAfter  :: Text
  }
  deriving (Eq, Show)

lcEmpty :: LineCursor
lcEmpty = LineCursor mempty mempty


lcTextWidth :: LineCursor -> Int
lcTextWidth LineCursor{..} = textWidth lineBefore


lcToText :: LineCursor -> Text
lcToText LineCursor{..} = lineBefore <> lineAfter

lcInsert :: Char -> LineCursor -> LineCursor
lcInsert c LineCursor{..} = LineCursor before' lineAfter
  where
    before' = lineBefore `snoc` c

lcAddWord :: Text -> LineCursor -> LineCursor
lcAddWord wrd LineCursor{..} = LineCursor before' lineAfter
  where
    before' = lineBefore <> wrd


lcPrev :: LineCursor -> LineCursor
lcPrev LineCursor{..} = LineCursor before after
  where
    (before, after) =
      case unsnoc lineBefore of
        Just (bf, c) -> (bf, cons c lineAfter)
        Nothing      -> (mempty, lineAfter)

lcNext :: LineCursor -> LineCursor
lcNext LineCursor{..} = LineCursor before after
  where
    (before, after) =
      case uncons lineAfter of
        Just (c, af) -> (snoc lineBefore c, af)
        Nothing      -> (lineBefore, lineAfter)

lcDeleteBack :: LineCursor -> LineCursor
lcDeleteBack LineCursor{..} = LineCursor before lineAfter
  where
    before =
      if Text.null lineBefore then mempty else
        Text.init lineBefore


lcDeleteForward :: LineCursor -> LineCursor
lcDeleteForward LineCursor{..} = LineCursor lineBefore after
  where
    after = if Text.null lineAfter then mempty else
              Text.tail lineAfter


lcTextStart :: LineCursor -> LineCursor
lcTextStart LineCursor{..} = LineCursor before mempty
  where
    before = lineBefore <> lineAfter

lcTextEnd :: LineCursor -> LineCursor
lcTextEnd LineCursor{..} = LineCursor mempty after
  where
    after = lineBefore <> lineAfter

lcDeleteAll :: LineCursor -> LineCursor
lcDeleteAll _ = LineCursor mempty mempty
