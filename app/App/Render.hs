module App.Render where

import Game.GameState.Passage
import Data.Text as T

tshow :: Show a => a -> Text
tshow = pack . show

renderChoices :: [Choice] -> Text
renderChoices choices = T.unlines $ 
  (\(indx, choice) -> tshow indx <> ") " <> choiceText choice) <$> Prelude.zip ([1..] :: [Integer]) choices

renderPassage :: Passage -> Text
renderPassage passage =
 passageTitle passage
  <> "\n"
  <> "\n"
  <> passageBody passage
  <> "\n"
  <> "\n"
  <> renderChoices (passageChoices passage)
