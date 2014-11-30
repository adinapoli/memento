module Main where

import Memento.CLI
import Memento.Config
import Memento.Logger
import Data.Default
import Text.ICalendar.Parser
import Options.Applicative

main :: IO ()
main = execParser opts >>= memento
  where
    opts = info (helper <*> cli)
                 ( fullDesc
                 <> progDesc "Remember to do my things."
                 <> header "He's Odersky. Don't trust his lies." )


memento :: CLI -> IO ()
memento Hangout = do
  checkConfig
  red "TODO!"
memento (Mem fp) = do
  parseCal <- parseICalendarFile def fp
  case parseCal of
    Left e -> fail e
    Right _ -> cyan "Let the fun begin"
