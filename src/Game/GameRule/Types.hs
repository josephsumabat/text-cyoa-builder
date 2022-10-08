module Game.GameRule.Types where

import Game.GameState.GameVar

-- | A rule that determines whether a choice shows up
data ConditionRule =
  AlwaysChoiceRule
    | NeverChoiceRule
    | AndChoiceRule [ConditionRule]
    | OrChoiceRule [ConditionRule]
    | StateChoiceRule GameVarKey Ordering GameVarKey
    deriving (Eq, Show, Read)

data ModifyOp =
  AddVarOp
    | MultVarOp
    deriving (Eq, Show, Read)

-- | A rule that determines whether a choice modifies game variables
data StateRule =
  NoOpState
    | SetVarStateRule GameVarVal GameVarKey
    | ModifyVarStateRule ModifyOp GameVarVal GameVarKey
    deriving (Eq, Show, Read)

