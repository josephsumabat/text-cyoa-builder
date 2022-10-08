module Game.Exception where

import Control.Monad.Catch
import Data.Text
import Game.GameState.Types
import Game.GameState.Passage

newtype VarLookupException = VarLookupException
  {
    varName :: Text
  }
  deriving (Show, Eq)

instance Exception VarLookupException

newtype PassageLookupException = PassageLookupException
  {
    passageName :: Text
  }
  deriving (Show, Eq)

instance Exception PassageLookupException

data ChoiceException = InvalidChoiceException Choice GameState
  | ChoiceVarLookupException VarLookupException
  | ChoicePassageLookupException PassageLookupException
  deriving (Show, Eq)

instance Exception ChoiceException
