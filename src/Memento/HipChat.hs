{-# LANGUAGE TemplateHaskell #-}
module Memento.HipChat where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Default
import Data.Monoid
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Control.Monad
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Aeson as JSON

-- | Simple HipChat notifier module for Memento.

--------------------------------------------------------------------------------
apiToken :: Text
apiToken = mempty

--------------------------------------------------------------------------------
data HCRoom =
      Dev
    | Releases deriving (Show, Eq)

--------------------------------------------------------------------------------
data HCColour = HC_Purple deriving (Show, Eq)

deriveJSON defaultOptions ''HCColour

--------------------------------------------------------------------------------
data HCPayload = HCPayload {
        color :: HCColour
      , message :: Text
      , notify :: Bool
      }

instance Default HCPayload where
    def = HCPayload HC_Purple mempty True

deriveJSON defaultOptions ''HCPayload


--------------------------------------------------------------------------------
renderRoom :: HCRoom -> Text
renderRoom Dev = "Development"
renderRoom Releases = "Releases"

--------------------------------------------------------------------------------
apiUrl :: Text
apiUrl = "https://api.hipchat.com"

--------------------------------------------------------------------------------
-- | Notify something inside HipChat
notifyRoom :: HCRoom -> Text -> IO ()
notifyRoom room msg = do
  rq' <- parseUrl $ T.unpack $ apiUrl <> "/v2/room/" <> renderRoom room <> "/notification"
  let headers = [(hContentType, "application/json")
              ,(hUserAgent, "memento/0.0.0")
              ,(hAuthorization, "Bearer " <> TE.encodeUtf8 apiToken)]
  let jsonBody = JSON.encode $ def { message = msg }
  let rq = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      , secure = True
      }
  void $ withManager defaultManagerSettings $ httpLbs rq
