module Main where

import Memento.CLI
import Memento.Logger
import Memento.Types
import Memento.GoogleCalendar
import Memento.HipChat
import Memento.Commands
import Data.Default
import Text.ICalendar.Parser
import Options.Applicative
import Control.Monad
import System.Exit
import System.IO
import qualified Data.Text as T


--------------------------------------------------------------------------------
main :: IO ()
main = execParser opts >>= memento
  where
    opts = info (helper <*> cli)
                 ( fullDesc
                 <> progDesc "Remember to do my things."
                 <> header "He's Odersky. Don't trust his lies." )

--------------------------------------------------------------------------------
repl :: IO ()
repl = do
  cyan "This is the memento REPL."
  cyan "Write :h to access the list of commands you can type."
  hSetBuffering stdout NoBuffering
  startMemento $ forever go
  where
    go :: Memento ()
    go = do
      liftIO $ hPutStr stdout "> "
      userAction <- liftIO getLine
      let uCommand = parseCommand userAction
      case uCommand of
        Nothing -> do
          liftIO $ yellow "eh?"
          go
        Just cmd -> processCommand cmd >> go

--------------------------------------------------------------------------------
memento :: CLI -> IO ()
memento Interactive = repl
memento Hangout = startMemento $ do
  link <- newHangoutLink "Morning Standup"
  case link of
    Left e -> liftIO $ red (T.pack . show $ e)
    Right hLink -> do
      liftIO (putStrLn "Hangout link: " >> green hLink)
      let msg = "@all The Standup Hangout link: " <> hLink <> " (via Memento)"
      notifyRoom DevChat msg


memento (Mem fp) = do
  parseCal <- parseICalendarFile def fp
  case parseCal of
    Left e -> fail e
    Right _ -> cyan "Let the fun begin"
