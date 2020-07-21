{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Border


import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input


import Data.Text


-- import Core.PrettyPrint
import Core.Parser
import Core.Expression



import Cursor

main :: IO ()
main = do
  defaultMain app initialState
  pure ()

initialState :: LineCursor
initialState =
  LineCursor mempty mempty


type Tick = ()
type Name = ()

app :: App LineCursor () Text
app = App { appDraw = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure
          , appAttrMap = \lc -> attrMap Vty.defAttr []
          }

handleEvent :: LineCursor -> BrickEvent Text () -> EventM Text (Next LineCursor)
handleEvent lc =
  let
    mDo func = continue $ func lc
    handleKey key =
      case key of
        KChar c -> mDo $ lcInsert c
        KLeft   -> mDo lcPrev
        KRight  -> mDo lcNext
        KBS     -> mDo lcDeleteBack
        KDel    -> mDo lcDeleteForward
        KEsc    -> halt lc
        KEnter  -> halt lc
        _ -> continue lc
  in
  \case
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          case elem MCtrl mods of
            True ->
              case key of
                KChar c | c == 'a' -> mDo lcTextEnd
                KChar c | c == 'e' -> mDo lcTextStart
                KChar c | c == 'k' -> mDo lcDeleteAll
                key ->  handleKey key
            _ -> handleKey key
        _ -> continue lc
    _ -> continue lc

draw :: LineCursor -> [Widget Text]
draw lc =
  let
    textWindow =
      borderWithLabel (withAttr "title" $ txt " Text")$
      viewport "editor" Both $
--      Brick.centerLayer $
      visible $
--      padAll 1 $
--      setAvailableSize (50, 50) $
      showCursor (pack "cursor") (Location (lcTextWidth lc, 0)) $
      txt (lcToText lc)
    helpWindow =
      borderWithLabel (withAttr "title" $ txt " Context")$
      vLimitPercent 30 $
      viewport "context" Both $
      visible $
      txt ""
  in
    [ textWindow <=> hBorder <=> helpWindow
    ]
