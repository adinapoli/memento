{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Memento.GoogleCalendar
    ( newHangoutLink
    ) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Monoid
import Data.Time
import System.Locale
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Memento.Types

--------------------------------------------------------------------------------
calendarId_ :: Memento Text
calendarId_ = requireEnv "memento.google.calendarId"

--------------------------------------------------------------------------------
calendarToken_ :: Memento Text
calendarToken_ = requireEnv "memento.google.apiToken"

--------------------------------------------------------------------------------
calendarAPI :: Text
calendarAPI = "https://www.googleapis.com/calendar/v3/calendars/"

--------------------------------------------------------------------------------
newtype GoogleDate = GoogleDate { date :: Text } deriving (Show, Eq)

deriveToJSON defaultOptions ''GoogleDate

--------------------------------------------------------------------------------
type CalendarId = Text

--------------------------------------------------------------------------------
data CalendarEventReq = CalendarEventReq {
        summary :: Text
      , start   :: GoogleDate
      , end     :: GoogleDate
      } deriving (Show, Eq)

deriveToJSON defaultOptions ''CalendarEventReq

--------------------------------------------------------------------------------
data CalendarEvent = CalendarEvent {
        ce_id :: Text
      , ce_hangoutLink :: Text
      , ce_iCalUID :: Text
      , ce_htmlLink :: Text
      } deriving (Show, Eq)


deriveFromJSON defaultOptions { fieldLabelModifier = drop 3 } ''CalendarEvent

--------------------------------------------------------------------------------
-- | Insert a new event inside the given calendar.
newEvent :: CalendarId -> CalendarEventReq -> Memento (Either String CalendarEvent)
newEvent cId cev@CalendarEventReq{..}= do
  rq' <- liftIO $ parseUrl $ T.unpack $ calendarAPI <> cId <> "/events"
  calendarToken <- calendarToken_
  let headers = [(hContentType, "application/json")
               ,(hUserAgent, "memento/0.0.0")
               ,(hAuthorization, "Bearer " <> TE.encodeUtf8 calendarToken)]
  let jsonBody = JSON.encode cev
  let rq = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      }
  res <- responseBody <$> liftIO (withManager tlsManagerSettings (httpLbs rq))
  return $ JSON.eitherDecode res

--------------------------------------------------------------------------------
newCalendarEvtRq :: Text -> Memento CalendarEventReq
newCalendarEvtRq title = do
  now <- liftIO getCurrentTime
  let googleAcceptedDate = formatTime defaultTimeLocale "%F" now
  return $ CalendarEventReq {
        summary = title
      , start   = GoogleDate $ T.pack googleAcceptedDate
      , end     = GoogleDate $ T.pack googleAcceptedDate
      }

--------------------------------------------------------------------------------
newHangoutLink :: Text -> Memento (Either String Text)
newHangoutLink title = do
  req <- newCalendarEvtRq title
  calendarId <- calendarId_
  newEvent calendarId req >>= return . either Left (Right . ce_hangoutLink)
