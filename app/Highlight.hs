{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BangPatterns      #-}

module Highlight where

import Data.Text as Text
import Brick
import Brick.Widgets.Core
import Graphics.Vty.Attributes as Vty

import Data.Text.Internal
import Data.Text.Unsafe
import Data.Char


data HighlightType  = HTVar | HTKeyword | None

hlTypeToAttrName :: HighlightType -> AttrName
hlTypeToAttrName =
  \case
    HTVar     -> "var"
    HTKeyword -> "keyword"
    None      -> ""

type HighlightText = [(Text, HighlightType)]

keywords :: [Text]
keywords = ["intros", "cal", "alix"]

renderText :: Text -> Widget Text
renderText = htRender . textToHighlightText


-- to do: handle new lines by doing lines and unlines outside
textToHighlightText :: Text -> HighlightText
textToHighlightText = fmap (either handleWS highlight) . wsSplit
  where
    highlight :: Text -> (Text, HighlightType)
    highlight t =
      case t `elem` keywords of
        True -> (t , HTKeyword)
        _    -> (t , HTVar)

    handleWS :: Text -> (Text, HighlightType)
    handleWS t = (t, None)

htRender :: HighlightText -> Widget Text
htRender =
    Prelude.foldr (\(t,ht) acc -> (renderWord (t, ht)) <+> acc) emptyWidget
  where
    renderWord :: (Text, HighlightType) -> Widget Text
    renderWord (t, ht) =
        withAttr (hlTypeToAttrName ht)
      $ txt t
      
    
wsSplit :: Text -> [Either Text Text]
wsSplit t@(Text arr off len)
  | Text.null t           = []
  | isSpace (Text.head t) = loop 0 0 True
  | otherwise             = loop 0 0 False
  where
    loop !start !n !wasSpace
        | n >= len = if wasSpace
                     then [Left  $ Text arr (start+off) (n-start)]
                     else [Right $ Text arr (start+off) (n-start)]
        | isSpace c && wasSpace = loop start (n + d) True
        | wasSpace  = Left  (Text arr (start+off) (n-start)) : loop n (n+d) False
        | isSpace c = Right (Text arr (start+off) (n-start)) : loop n (n+d) True
        | otherwise = loop start (n+d) False
        where Iter c d = iter t n
              
