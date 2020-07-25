{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Highlight where

import Data.Text as Text
import Brick
import Brick.Widgets.Core
import Graphics.Vty.Attributes as Vty


data HighlightType  = HTVar | HTKeyword

hlTypeToAttrName :: HighlightType -> AttrName
hlTypeToAttrName =
  \case
    HTVar     -> "var"
    HTKeyword -> "keyword"

type HighlightText = [(Text, HighlightType)]

keywords :: [Text]
keywords = ["intros", "cal", "alix"]

renderText :: Text -> Widget Text
renderText = htRender . textToHighlightText


-- to do: handle new lines by doing lines and unlines outside
textToHighlightText :: Text -> HighlightText
textToHighlightText = fmap highlight . Text.words
  where
    highlight :: Text -> (Text, HighlightType)
    highlight t =
      case t `elem` keywords of
        True -> (t `snoc` ' ', HTKeyword)
        _    -> (t `snoc` ' ', HTVar)

htRender :: HighlightText -> Widget Text
htRender =
    Prelude.foldr (\(t,ht) acc -> (renderWord (t, ht)) <+> acc) emptyWidget

  where
    renderWord :: (Text, HighlightType) -> Widget Text
    renderWord (t, ht) =
        withAttr (hlTypeToAttrName ht)
      $ txt t
      
    

