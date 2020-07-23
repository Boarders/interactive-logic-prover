{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
  { edText :: LineCursor
  , edMode :: EdMode
  }

initialState :: EdState
initialState = EdState txt mode
  where
    txt  = LineCursor mempty mempty
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
    KChar c | c == 'a'  -> mDo lcTextEnd
    KChar c | c == 'e'  -> mDo lcTextStart
    KChar c | c == 'k'  -> mDo lcDeleteAll
    key ->  normalInput edState key
  where
    mDo func = continue $ EdState (func lc) mode


normalInput :: EdState -> Key -> EventM Text (Next EdState)
normalInput edState@(EdState lc mode) =
  \case
    KChar c | c == '\\' -> continue $ EdState lc EdSpec    
    KChar c -> mDo $ lcInsert c
    KLeft   -> mDo lcPrev
    KRight  -> mDo lcNext
    KBS     -> mDo lcDeleteBack
    KDel    -> mDo lcDeleteForward
    KEsc    -> halt edState
    KEnter  -> halt edState
    _ -> continue edState
  where
    mDo func = continue $ EdState (func lc) mode    

specialInput :: EdState -> Key -> EventM Text (Next EdState)
specialInput edState@(EdState lc mode) key =
  case key of
    KChar c ->
      case Map.lookup c specialInputMap of
        Just spec -> mDo $ lcAddWord spec
        Nothing   -> normalInput (edState{edMode = EdOrd}) key
    _ -> normalInput edState key
  where
    mDo func = continue $ EdState (func lc) EdOrd

draw :: EdState -> [Widget Text]
draw (EdState lc mode) =
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


specialInputMap :: Map.Map Char Text
specialInputMap =
  Map.fromList
    [('i', "intros")]
  
