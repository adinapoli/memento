module Main where

import Memento.CLI
import Memento.Config
import Memento.Logger
import Memento.Types
import Memento.GoogleCalendar
import Memento.HipChat
import Data.Default
import Text.ICalendar.Parser
import Options.Applicative
import qualified Data.Text as T

main :: IO ()
main = execParser opts >>= memento
  where
    opts = info (helper <*> cli)
                 ( fullDesc
                 <> progDesc "Remember to do my things."
                 <> header "He's Odersky. Don't trust his lies." )


memento :: CLI -> IO ()
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
