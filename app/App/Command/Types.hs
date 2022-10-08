module App.Command.Types where

import Game.GameState.Passage
import Data.Text

data Command =
  LoadGame FilePath
    | EditGame
    | EditPassageBody PassageKey Text
    | AddChoice PassageKey PassageKey Text
    | StartGame
    | Save FilePath
    | MakeChoice Integer
    | Back
    | Help
    | Load FilePath
    | NewGame
    deriving (Eq, Show)

