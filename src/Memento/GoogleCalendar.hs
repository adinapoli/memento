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

--------------------------------------------------------------------------------
-- "mytest" calendar
calendarId :: Text
calendarId = "irisconnect.co.uk_gu08qbqf9githq086f405oe3po%40group.calendar.google.com"

--------------------------------------------------------------------------------
calendarToken :: Text
calendarToken = mempty

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
newEvent :: CalendarId -> CalendarEventReq -> IO (Either String CalendarEvent)
newEvent cId cev@CalendarEventReq{..}= do
  rq' <- parseUrl $ T.unpack $ calendarAPI <> cId <> "/events"
  let headers = [(hContentType, "application/json")
               ,(hUserAgent, "memento/0.0.0")
               ,(hAuthorization, "Bearer " <> TE.encodeUtf8 calendarToken)]
  let jsonBody = JSON.encode cev
  let rq = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      , secure = True
      }
  res <- responseBody <$> (withManager defaultManagerSettings (httpLbs rq))
  return $ JSON.eitherDecode res

--------------------------------------------------------------------------------
newCalendarEvtRq :: Text -> IO CalendarEventReq
newCalendarEvtRq title = do
  now <- getCurrentTime
  let googleAcceptedDate = formatTime defaultTimeLocale "%F" now
  return $ CalendarEventReq {
        summary = title
      , start   = GoogleDate $ T.pack googleAcceptedDate
      , end     = GoogleDate $ T.pack googleAcceptedDate
      }

--------------------------------------------------------------------------------
newHangoutLink :: Text -> IO (Either String Text)
newHangoutLink title = do
  req <- newCalendarEvtRq title
  newEvent calendarId req >>= return . either Left (Right . ce_hangoutLink)
