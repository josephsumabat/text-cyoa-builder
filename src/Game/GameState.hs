module Game.GameState
  where

import Data.Text
import Game.GameState.Types
import Game.GameState.Passage
import Game.GameState.GameVar
import Game.Exception
import GHC.Stack
import Game.GameRule
import Game.GameRule.Types
import Game.GameState.Environment
import Control.Monad
import Control.Monad.Trans.Except

-- | Update game state according to choice selection. Sets the passage and updates variables.
makeChoice :: HasCallStack => Choice -> GameState -> Except ChoiceException GameState
makeChoice choice state = do
  conditionSatisfied <- withExceptT ChoiceVarLookupException $
    reduceConditionRule (conditionRule choice) state
  if conditionSatisfied then do
    nextPassage <- withExceptT ChoicePassageLookupException $
      lookupPassage (choiceNextPassage choice) (statePassageEnv state)
    pure $ state {currentPassage = nextPassage }
  else
    throwE $ InvalidChoiceException choice state

-- | Get valid choices according to precondition rules
currentValidChoices :: GameState -> Except VarLookupException [Choice]
currentValidChoices state = do
  let allChoices  = passageChoices . currentPassage $ state
  filterM (\c -> reduceConditionRule (conditionRule c) state) allChoices

 -- -- | Edit a passage of a cyoa game.
 -- editPassage :: PassageKey -> CyoaGame -> Passage -> CyoaGame
 -- editPassage passageKey cyoaGame newPassage = do
 --   let gameState = initialState . unInitialGameState $ cyoaGame
 --       passageEnv = statePassageEnv gameState
 --   oldPassage  <- lookupPassage passageKey passageEnv
 --   updatedCyoaGame = (overInitialGameState . overPassageEnv) (putPassageKey p) 

addChoice :: PassageKey -> PassageKey -> Text -> CyoaGame -> Except PassageLookupException CyoaGame
addChoice fromKey toKey choiceText cyoaGame = do
  let gameState = unInitialGameState . initialState $ cyoaGame
      passageEnv = statePassageEnv gameState
      newChoice = Choice toKey AlwaysChoiceRule NoOpState choiceText
  fromPassage <- lookupPassage fromKey passageEnv
  toPassage <- lookupPassage toKey passageEnv
  let updatedFromPassage = (overPassageChoices (newChoice:)) fromPassage
  pure $ (overInitialState . overUnInitialGameState . overCurrentPassage) (const updatedFromPassage) $ cyoaGame

newGameState :: GameState
newGameState =
  GameState
    {
      statePassageEnv = putPassage "start" newPassage emptyPassageEnv
    , stateVarEnv = emptyVarEnv
    , currentPassage = newPassage
    , prevState = Nothing
    }

newCyoaGame :: CyoaGame
newCyoaGame =
  CyoaGame
    {
      gameTitle = "Replace me game title"
    , initialState = InitialGameState newGameState
    }
