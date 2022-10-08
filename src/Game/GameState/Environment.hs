module Game.GameState.Environment where

import GHC.Stack
import Game.Exception
import Game.GameState.Passage
import Game.GameState.GameVar
import Data.Map qualified as Map
import Control.Monad.Trans.Except

-- Maybe environment should be a typeclass instead of a concrete map implementation?

lookupPassage :: HasCallStack => PassageKey -> PassageEnvironment -> Except PassageLookupException Passage
lookupPassage passageKey passageEnv = maybe (throwE $ PassageLookupException passageKey) pure (Map.lookup passageKey passageEnv)

putPassage :: HasCallStack => PassageKey -> Passage -> PassageEnvironment -> PassageEnvironment
putPassage =  Map.insert

lookupVar :: HasCallStack => GameVarKey -> VarEnvironment -> Except VarLookupException GameVarVal
lookupVar varKey varEnv = maybe (throwE $ VarLookupException varKey) pure (Map.lookup varKey varEnv)

putVar :: HasCallStack => GameVarKey -> GameVarVal -> VarEnvironment -> VarEnvironment
putVar =  Map.insert
