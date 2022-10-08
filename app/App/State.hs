{-# Language TemplateHaskell #-}
module App.State where

import Game.GameState.Types
import Data.Derive.OverField

newtype PlayState =
  PlayState
    {
      currentPlayGame :: GameState
    }
  deriving (Eq, Show)

data EditState =
  EditState
    {
      currentEditGame :: CyoaGame
    }
  deriving (Eq, Show)

$(deriveOverAllFields OverField ''EditState)

data CyoaCliState = CyoaCliInitialState
  | CyoaCliGameLoadedState { loadedGame :: CyoaGame }
  | CyoaCliEditState EditState
  | CyoaCliPlayState PlayState
  deriving (Eq, Show)

