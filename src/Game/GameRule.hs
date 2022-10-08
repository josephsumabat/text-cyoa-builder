module Game.GameRule where

import Control.Monad.Catch
import GHC.Stack
import Game.GameRule.Types
import Game.GameState.Types
import Game.GameState.GameVar
import Game.GameState.Environment
import Game.Exception
import Control.Monad.Trans.Except

-- | reduce conditional rule. Does not modify game state
reduceConditionRule :: HasCallStack => ConditionRule -> GameState -> Except VarLookupException Bool
reduceConditionRule AlwaysChoiceRule _ = pure True
reduceConditionRule NeverChoiceRule _ = pure False
reduceConditionRule (AndChoiceRule rules) state = and <$> mapM (`reduceConditionRule` state) rules
reduceConditionRule (OrChoiceRule rules) state = or <$> mapM (`reduceConditionRule` state) rules
reduceConditionRule (StateChoiceRule varKey1 ordering varKey2) state =
  do
    let varEnv = stateVarEnv state
    varVal1 <- lookupVar varKey1 varEnv
    varVal2 <- lookupVar varKey2 varEnv
    pure $ compare varVal1 varVal2 == ordering

-- Not yet implemented
updateGameVarsFromRule :: (HasCallStack, MonadThrow m) => StateRule -> GameState -> m VarEnvironment
updateGameVarsFromRule _ state = pure (stateVarEnv state)
