
module Memento.Commands where

import System.Exit
import Memento.Types

data MementoCommand =
  MementoExit
  deriving (Show, Eq)

parseCommand :: String -> Maybe MementoCommand
parseCommand ":q" = Just MementoExit
parseCommand _ = Nothing


processCommand :: MementoCommand -> Memento ()
processCommand MementoExit = liftIO exitSuccess
