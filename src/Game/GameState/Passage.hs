{-# Language TemplateHaskell #-}
module Game.GameState.Passage where

import Data.Text
import Data.Map as Map
import Data.Derive.OverField
import Game.GameRule.Types
import Control.Lens.Combinators hiding (Choice)

type PassageKey = Text

-- Alternatively this could be represented as a directed adjacency list instead
data Choice =
  Choice
    {
      choiceNextPassage :: PassageKey
    , conditionRule :: ConditionRule
    , stateRule :: StateRule
    , choiceText :: Text
    }
  deriving (Eq, Show, Read)

$(deriveOverAllFields OverField ''Choice)

data Passage =
  Passage
    {
      passageTitle :: Text
    , passageBody :: Text
    , passageChoices :: [Choice]
    }
  deriving (Eq, Show, Read)

$(deriveOverAllFields OverField ''Passage)

type PassageEnvironment = Map PassageKey Passage

emptyPassageEnv :: PassageEnvironment
emptyPassageEnv = Map.empty

newPassage :: Passage
newPassage =
  Passage {
    passageTitle = "Placeholder Title"
    , passageBody = "Lorem Ipsum"
    , passageChoices = []
  }
