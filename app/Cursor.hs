{-# LANGUAGE RecordWildCards #-}

module Cursor where

import Data.Text as Text
import Brick.Widgets.Core

data DocCursor =
  DocCursor
  { docBefore :: Text
  , docCurr   :: LineCursor
  , docAfter  :: Text
  }
  deriving (Eq, Show)

data LineCursor =
  LineCursor
  { lineBefore :: Text
  , lineAfter  :: Text
  }
  deriving (Eq, Show)

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
    before = Text.init lineBefore


lcDeleteForward :: LineCursor -> LineCursor
lcDeleteForward LineCursor{..} = LineCursor lineBefore after
  where
    after = Text.tail after


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
