{-# LANGUAGE QuasiQuotes #-}
module Memento.Config 
  (checkConfig) where

import Data.Char
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import Text.RawString.QQ
import Memento.Logger
import Shelly


--------------------------------------------------------------------------------
checkConfig :: IO ()
checkConfig = shelly $ silently $ escaping False $ do
  homePath <- T.init <$> run "echo" ["$HOME"]
  let cfgFolder = fromText $ homePath <> "/.memento"
  let cfgPath = cfgFolder </> (fromText "memento.cfg")
  cfgInMemPath <- test_f cfgPath
  unless cfgInMemPath $ do
    liftIO $ do
      cyan "Memento couldn't found a config file!"
      cyan "Would you like to create a new one for you? [y/n]"
    userWill <- liftIO getLine
    case map toLower userWill of
      "y" -> do
        mkdir_p cfgFolder
        writefile cfgPath (T.pack configTemplate)
        liftIO $ green "Config file successfully created!"
      _ -> do
        liftIO $ red "Ok, memento won't create a config file for you..."
        fail "Memento cannot continue without a valid config file."

--------------------------------------------------------------------------------
configTemplate :: String
configTemplate = [r|
  memento {
    google {
      calendarId = "valid-calendar-id"
      apiToken = "your-api-token"
    }
    hipchat {
      apiToken = "your-hipchat-token"
    }
  }
|]
