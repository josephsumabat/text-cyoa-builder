module Main where

import App.Command
import App.Command.Parse
import App.State
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Text.IO qualified as T
import System.IO

main :: IO ()
main = do
  putStrLn "Choose your own adventure started! type \"help\" for help!"
  evalStateT readTillEOF CyoaCliInitialState 

readTillEOF :: StateT CyoaCliState IO ()
readTillEOF = do
  done <- liftIO isEOF
  if done
    then liftIO $ putStrLn "Bye!"
    else
      do
        input <- liftIO T.getLine
        let mCommand = parseCommand input
        maybe (liftIO $ putStrLn "Not a valid command!") handleCommand mCommand
        readTillEOF

