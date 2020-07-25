{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Command where

import Data.Text as Text
import Brick.Widgets.Core

data Bwd a = Nil | Bwd a :|> a

data WordPos = WordPos
  { wpCommand :: Text
  , wpPos     :: !(Int, Int)
  }


type Commands = Bwd WordPos

cmEmpty = Nil
