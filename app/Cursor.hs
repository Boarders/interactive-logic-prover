{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}

module Cursor where

import Data.Text as Text
import Brick.Widgets.Core
import Data.Foldable (toList)

data Bwd a = Nil | Snoc (Bwd a) a
  deriving stock Functor
  deriving stock Foldable
  deriving stock (Eq, Show)

instance Semigroup (Bwd a) where
  (<>) = undefined

instance Monoid (Bwd a) where
  mempty = Nil

infixl 5 :|>
pattern (:|>) :: Bwd a -> a -> Bwd a
pattern xs :|> x <- Snoc xs x
  where
    xs :|> x = Snoc xs x
{-# COMPLETE Nil, (:|>) #-}

bwdToList :: Bwd a -> [a]
bwdToList = toList


data DocCursor =
  DocCursor
  { docBefore :: Bwd Text
  , docLineNo :: Int
  , docCurr   :: LineCursor
  , docAfter  :: [Text]
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
    before = (docBefore :|> lcToText docCurr)

-- to do: doc delete dealing with start of line and with first line

docToLines :: DocCursor -> [Text]
docToLines DocCursor{..} =
  linesBefore <> [(lcToText docCurr)] <> docAfter
  where
    linesBefore = bwdToList docBefore
    

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

docDeleteBack :: DocCursor -> DocCursor
docDeleteBack DocCursor{..} =
  let
    LineCursor{..} = docCurr
  in
    undefined
  


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
