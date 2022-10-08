module App.Command.Parse where

import App.Command.Types
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseCommand :: Text -> Maybe Command
parseCommand = parseMaybe commandParser

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first:rest)

-- TODO: handle backtracking failure
commandParser :: Parser Command
commandParser =
  StartGame <$ string "start"
  <|> MakeChoice <$ string "choose" <* spaceConsumer <*> L.lexeme spaceConsumer L.decimal
  <|> Back <$ string "back"
  <|> Help <$ string "help"
  <|> Load <$ string "load" <* spaceConsumer <*> (unpack <$> takeRest)
  <|> Save <$ string "save" <* spaceConsumer <*> (unpack <$> takeRest)
  <|> NewGame <$ string "new"
  -- <|> EditPassageBody <$ string "editbody" <* spaceConsumer <*> ( many1 alphaNumChar) <* spaceConsumer <*> (many1 alphaNumChar)

