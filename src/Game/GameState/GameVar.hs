module Game.GameState.GameVar where

import Data.Map as Map
import Data.Text

type GameVarKey = Text
type GameVarVal = Int

-- Maybe these could be a typeclass instead
type VarEnvironment = Map GameVarKey GameVarVal

emptyVarEnv :: VarEnvironment
emptyVarEnv = Map.empty
