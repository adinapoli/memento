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
import Memento.Types
import qualified Data.Aeson as JSON

-- | Simple HipChat notifier module for Memento.

--------------------------------------------------------------------------------
apiToken_ :: Memento T.Text
apiToken_ = requireEnv "memento.hipchat.apiToken"

--------------------------------------------------------------------------------
data HCRoom =
      Dev
    | Releases deriving (Show, Eq)

--------------------------------------------------------------------------------
data HCColour = HC_Purple deriving (Show, Eq)

instance JSON.ToJSON HCColour where
  toJSON HC_Purple = JSON.String "purple"

--------------------------------------------------------------------------------
data HCPayload = HCPayload {
        color :: HCColour
      , message :: Text
      , notify :: Bool
      }

instance Default HCPayload where
    def = HCPayload HC_Purple mempty True

deriveToJSON defaultOptions ''HCPayload


--------------------------------------------------------------------------------
renderRoom :: HCRoom -> Text
renderRoom Dev = "Development"
renderRoom Releases = "Releases"

--------------------------------------------------------------------------------
apiUrl :: Text
apiUrl = "http://api.hipchat.com"

--------------------------------------------------------------------------------
-- | Notify something inside HipChat
notifyRoom :: HCRoom -> Text -> Memento ()
notifyRoom room msg = do
  apiToken <- apiToken_
  rq' <- liftIO $ parseUrl $ T.unpack $ apiUrl <> "/v2/room/"
                                               <> renderRoom room
                                               <> "/notification"
  let headers = [(hContentType, "application/json")
              ,(hUserAgent, "memento/0.0.0")
              ,(hAuthorization, "Bearer " <> TE.encodeUtf8 apiToken)]
  let jsonBody = JSON.encode $ def { message = msg }
  let rq = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      }
  void $ liftIO $ withManager defaultManagerSettings $ httpLbs rq
