{-# LANGUAGE OverloadedStrings #-}
module Memento.CLI where

import Options.Applicative

data CLI =
  Mem FilePath

mem :: Parser CLI
mem = Mem <$> (strOption (long "mem" <> (metavar "FILEPATH/FILE.ICS")))
