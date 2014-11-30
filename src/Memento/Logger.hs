
module Memento.Logger where

import Data.Text (Text, unpack)
import System.Console.ANSI

--------------------------------------------------------------------------------
reset :: IO ()
reset = setSGR [Reset]

--------------------------------------------------------------------------------
colorised :: Color -> Text -> IO ()
colorised col msg = do
  setSGR [SetColor Foreground Vivid col]
  putStrLn . unpack $ msg
  reset

--------------------------------------------------------------------------------
green :: Text -> IO ()
green = colorised Green

--------------------------------------------------------------------------------
red :: Text -> IO ()
red = colorised Red

--------------------------------------------------------------------------------
yellow :: Text -> IO ()
yellow = colorised Red

--------------------------------------------------------------------------------
cyan :: Text -> IO ()
cyan = colorised Cyan
