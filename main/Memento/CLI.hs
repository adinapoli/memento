module Memento.CLI where

import Options.Applicative

data CLI =
    Mem FilePath
  | Interactive
  | Hangout deriving (Show, Eq)

mem :: Parser CLI
mem = Mem <$> strOption (long "mem" <> metavar "FILEPATH/FILE.ICS")

hangout :: Parser CLI
hangout = pure Hangout <* switch (long "hangout")

interactive :: Parser CLI
interactive = pure Interactive <* switch (short 'i')

cli :: Parser CLI
cli = foldr1 (<|>) [mem, hangout, interactive]
