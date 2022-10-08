module App.Command where

import App.Exception
import App.State
import App.Command.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Text
import Game.GameState
import Game.GameState.Environment
import Game.GameState.Passage
import Game.GameState.Types
import Game.GameRule.Types
import Game.Save
import App.Render
import Data.Text.IO qualified as T

type StateMessagePair = (CyoaCliState, Text)

handleCommand :: Command -> StateT CyoaCliState IO ()
handleCommand Help =
  liftIO $ putStrLn $
    "help - shows this page\n"
      <> "load - load a game from a file"
handleCommand NewGame = do
  put $ CyoaCliEditState (EditState newCyoaGame)
  liftIO $ putStrLn "Loaded a new game in edit mode. Use \"save <filename>\" to save."
handleCommand (Load filePath) =
  case filePath of
    "" -> liftIO $ putStrLn "Please enter a file name"
    file -> do
      liftIO $ putStrLn $ "Loading file: " <> file
      -- TODO: handle io exceptions
      mCyoaGame <- liftIO $ loadFromFile @CyoaGame file
      maybe (liftIO $ putStrLn "Could not read file!") (put . CyoaCliGameLoadedState) mCyoaGame
handleCommand cmd = do
  currState <- get
  case currState of
    CyoaCliInitialState -> liftIO $ putStrLn "Please load a file before trying to run that command!"
    CyoaCliGameLoadedState loadedCyoaGame -> handleCommandInLoadMode cmd loadedCyoaGame 
    CyoaCliPlayState playState -> handleCommandInPlayMode cmd playState
    CyoaCliEditState editState -> handleCommandInEditMode cmd editState

-- TODO: Return state in these functions since they shouldnt read state?
handleCommandInLoadMode :: Command -> CyoaGame -> StateT CyoaCliState IO ()
handleCommandInLoadMode StartGame loadedCyoaGame = do
  put . CyoaCliPlayState . PlayState . unInitialGameState . initialState $ loadedCyoaGame
  liftIO $ T.putStrLn $ renderPassage . currentPassage . unInitialGameState . initialState $ loadedCyoaGame
handleCommandInLoadMode _ _ = liftIO $ putStrLn "Command not available in load game mode!"

nextLoadState :: Command -> CyoaGame -> Except CommandException CyoaCliState
nextLoadState StartGame =  pure . CyoaCliPlayState . PlayState . unInitialGameState . initialState
nextLoadState EditGame =  pure . CyoaCliEditState . EditState
nextLoadState _ = const $ throwE CommandException

handleCommandInPlayMode :: Command -> PlayState -> StateT CyoaCliState IO ()
handleCommandInPlayMode (MakeChoice idx) playState =
  --Will use  make choice and current valid choices
  liftIO $ putStrLn "not implemented"
handleCommandInPlayMode _ _= liftIO $ putStrLn "Command not available in play game mode!"

handleCommandInEditMode :: Command -> EditState -> StateT CyoaCliState IO ()
handleCommandInEditMode (Save filepath) editState = do
  liftIO $ putStrLn $ "Saved game in file: " <> filepath
  -- TODO: handle IO exceptions
  liftIO $ saveToFile filepath (editState.currentEditGame)
handleCommandInEditMode (EditPassageBody passageKey newPassageBody) editState = do
  let passageEnv = editState.currentEditGame.initialState.unInitialGameState.statePassageEnv
  case runExcept (lookupPassage passageKey passageEnv) of
    Right p -> do
      let newEditState = (overCurrentEditGame . overInitialState . overUnInitialGameState . overStatePassageEnv) (putPassage passageKey p { passageBody =  newPassageBody}) $ editState
      put $ CyoaCliEditState newEditState
    Left _ -> liftIO $ putStrLn "passage key does not exist!"
handleCommandInEditMode (AddChoice k1 k2 newChoiceText) editState = do
  let eNewEditState = runExcept $ EditState <$> addChoice k1 k2 newChoiceText (currentEditGame editState)
  case eNewEditState of
        Right editState -> put (CyoaCliEditState editState)
        -- TODO: handle this properly (print the keys)
        Left e -> liftIO $ putStrLn "Error adding choice!"
handleCommandInEditMode _ _ = liftIO $ putStrLn "Command not available in edit game mode!"
