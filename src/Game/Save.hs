module Game.Save where

import Game.GameState.Types
import Text.Read qualified as T

class Serializable a where
  serialize :: a -> String
  unserialize :: String -> Maybe a

instance Serializable CyoaGame where
  serialize = show
  unserialize = T.readMaybe

saveToFile :: Serializable a => FilePath -> a -> IO ()
saveToFile filepath serializableVal = writeFile filepath (serialize serializableVal)

-- Not concerned with throwing proper errors right now
loadFromFile :: Serializable a => FilePath -> IO (Maybe a)
loadFromFile filepath = unserialize <$> readFile filepath
