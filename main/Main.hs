module Main where

import Memento.CLI
import Data.Default
import Text.ICalendar.Parser
import Options.Applicative

main :: IO ()
main = execParser opts >>= memento
  where
    opts = info (helper <*> mem)
                 ( fullDesc
                 <> progDesc "Remember to do my things."
                 <> header "He's Odersky. Don't trust his lies." )


memento :: CLI -> IO ()
memento (Mem fp) = do
  parseCal <- parseICalendarFile def fp
  case parseCal of
    Left e -> fail e
    Right _ -> putStrLn "Let the fun begin"
