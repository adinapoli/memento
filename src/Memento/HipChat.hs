{-# LANGUAGE TemplateHaskell #-}
module Memento.HipChat where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Default
import Data.Monoid
import Control.Applicative
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Control.Monad
import Network.HTTP.Client
import Network.HTTP.Types
import Memento.Types

-- | Simple HipChat notifier module for Memento.

--------------------------------------------------------------------------------
apiToken_ :: Memento T.Text
apiToken_ = requireEnv "memento.hipchat.apiToken"

--------------------------------------------------------------------------------
me_ :: Memento T.Text
me_ = requireEnv "memento.hipchat.me"

--------------------------------------------------------------------------------
data HCRoom =
      Development
    | DevChat
    | Releases deriving (Show, Eq)

--------------------------------------------------------------------------------
data HCStatus =
      Available
    | Idle
    | DoNotDisturb deriving (Show, Eq)

instance JSON.ToJSON HCStatus where
  toJSON Available = "available"
  toJSON Idle = "idle"
  toJSON DoNotDisturb = "dnd"

data HCPresence = HCPresence {
       hcp_status :: Text
     , hcp_is_online :: Bool
     , hcp_show :: Maybe HCStatus
     }

deriveToJSON defaultOptions { fieldLabelModifier = drop 4 } ''HCPresence

--------------------------------------------------------------------------------
data HCColour = HC_Purple deriving (Show, Eq)

instance JSON.ToJSON HCColour where
  toJSON HC_Purple = JSON.String "purple"

--------------------------------------------------------------------------------
data HCPayload = HCPayload {
        color :: HCColour
      , message :: Text
      , notify :: Bool
      , message_format :: Text
      }

instance Default HCPayload where
    def = HCPayload HC_Purple mempty True "text"

deriveToJSON defaultOptions ''HCPayload


--------------------------------------------------------------------------------
renderRoom :: HCRoom -> Text
renderRoom Development = "Development"
renderRoom DevChat = "dev-chat"
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

--------------------------------------------------------------------------------
me :: Memento (Either String JSON.Value)
me = do
  apiToken <- apiToken_
  myEmail <- me_
  rq' <- liftIO $ parseUrl $ T.unpack $ apiUrl <> "/v2/user/"
                                               <> myEmail
  let headers = [(hContentType, "application/json")
              ,(hUserAgent, "memento/0.0.0")
              ,(hAuthorization, "Bearer " <> TE.encodeUtf8 apiToken)]
  let rq = rq' {
        method = "GET"
      , requestHeaders = headers
      }
  res <- responseBody <$> liftIO (withManager defaultManagerSettings (httpLbs rq))
  return $ JSON.eitherDecode res

--------------------------------------------------------------------------------
setStatus :: HCPresence -> Memento ()
setStatus pres = do
  apiToken <- apiToken_
  myEmail <- me_
  rq' <- liftIO $ parseUrl $ T.unpack $ apiUrl <> "/v2/user/"
                                               <> myEmail
  let headers = [(hContentType, "application/json")
              ,(hUserAgent, "memento/0.0.0")
              ,(hAuthorization, "Bearer " <> TE.encodeUtf8 apiToken)]
  let jsonBody = JSON.encode pres
  let rq = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      }
  void $ liftIO $ withManager defaultManagerSettings $ httpLbs rq
