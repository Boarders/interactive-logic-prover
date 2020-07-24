{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Border


import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input


import Data.Text
import qualified Data.Map as Map

-- import Core.PrettyPrint
import Core.Parser
import Core.Expression



import Cursor

main :: IO ()
main = do
  defaultMain app initialState
  pure ()


data EdMode = EdOrd | EdSpec

data EdState = EdState
  { edText :: DocCursor
  , edMode :: EdMode
  }

initialState :: EdState
initialState = EdState txt mode
  where
    txt  = docEmpty
    mode = EdOrd


app :: App EdState () Text
app = App { appDraw = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure
          , appAttrMap = \lc -> attrMap Vty.defAttr []
          }

handleEvent :: EdState -> BrickEvent Text () -> EventM Text (Next EdState)
handleEvent edState@(EdState lc mode) =
  \case
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          case mode of
            EdSpec -> specialInput edState key
            _ ->
              case elem MCtrl mods of
                True -> ctrlInput edState key
                _ -> normalInput edState key
        _ -> continue edState
    _ -> continue edState



ctrlInput :: EdState -> Key -> EventM Text (Next EdState)
ctrlInput edState@(EdState lc mode) =
  \case
    KChar c | c == 'a'  -> mDo (docEdLine lcTextEnd)
    KChar c | c == 'e'  -> mDo (docEdLine lcTextStart)
    KChar c | c == 'k'  -> mDo (docEdLine lcDeleteAll)
    key ->  normalInput edState key
  where
    mDo func = continue $ EdState (func lc) mode


normalInput :: EdState -> Key -> EventM Text (Next EdState)
normalInput edState@(EdState doc mode) =
  \case
    KChar c | c == '\\' -> continue $ EdState doc EdSpec
    KChar c -> mDo $ (docEdLine (lcInsert c))
    KLeft   -> mDo (docEdLine lcPrev)
    KRight  -> mDo (docEdLine lcNext)
    -- to do: handle these properly for doc type with no curr LC
    KBS     -> mDo (docEdLine lcDeleteBack)
    KDel    -> mDo (docEdLine lcDeleteForward)
    KEsc    -> halt edState
    KEnter  -> mDo docNewLine
    _ -> continue edState
  where
    mDo func = continue $ EdState (func doc) mode

specialInput :: EdState -> Key -> EventM Text (Next EdState)
specialInput edState@(EdState lc mode) key =
  case key of
    KChar c ->
      case Map.lookup c specialInputMap of
        Just spec -> mDo $ (docEdLine (lcAddWord spec))
        Nothing   -> normalInput (edState{edMode = EdOrd}) key
    _ -> normalInput edState key
  where
    mDo func = continue $ EdState (func lc) EdOrd

draw :: EdState -> [Widget Text]
draw (EdState doc mode) =
  let
    textWindow =
      borderWithLabel (withAttr "title" $ txt " Text")$
      viewport "editor" Both $
--      Brick.centerLayer $
      visible $
      drawDoc
      doc
--      padAll 1 $
--      setAvailableSize (50, 50) $
    helpWindow =
      borderWithLabel (withAttr "title" $ txt " Context")$
      vLimitPercent 30 $
      viewport "context" Both $
      visible $
      txt ""
  in
    [ textWindow <=> hBorder <=> helpWindow
    ]


drawDoc :: DocCursor -> Widget Text
drawDoc doc@DocCursor{..} =
  showCursor (pack "cursor") (Location (lcTextWidth lc, docLineNo)) $
  txt (docToText doc)
  where
    lc = docCurr


specialInputMap :: Map.Map Char Text
specialInputMap =
  Map.fromList
    [('i', "intros")]
