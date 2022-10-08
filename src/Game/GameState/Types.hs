{-# Language TemplateHaskell #-}
module Game.GameState.Types
  where

import Data.Text
import Data.Derive.OverField
import Game.GameState.GameVar
import Game.GameState.Passage
import Control.Lens.Combinators

--  In practice I would wantto hide this module
--

-- | Game state of a running game
-- The story graph is represented by 
data GameState =
  GameState
    {
    -- Wonder if there's a more type safe way to do this or if this is the best
    -- graph representation
    -- | statePassageEnv should not change at runtime during playing of a game
    -- but only
      statePassageEnv :: PassageEnvironment
    , stateVarEnv :: VarEnvironment
    , currentPassage :: Passage
    , prevState :: Maybe GameState
    }
    deriving (Eq, Show, Read)

$(deriveOverAllFields OverField ''GameState)

-- | Represents a game before it has started. Current passage would be the starting passage
data InitialGameState = InitialGameState { unInitialGameState :: GameState }
  deriving (Eq, Show, Read)

$(deriveOverAllFields OverField ''InitialGameState)

data CyoaGame =
  CyoaGame
    {
      gameTitle :: Text  
    , initialState :: InitialGameState
    }
  deriving (Eq, Show, Read)

$(deriveOverAllFields OverField ''CyoaGame)
