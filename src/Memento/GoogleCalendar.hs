{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Memento.GoogleCalendar
    ( newHangoutLink
    ) where

import Data.Text
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Date.Time

--------------------------------------------------------------------------------
-- "mytest" calendar
calendarId :: Text
calendarId = "irisconnect.co.uk_gu08qbqf9githq086f405oe3po%40group.calendar.google.com"

--------------------------------------------------------------------------------
calendarToken :: Text
calendarToken = _

--------------------------------------------------------------------------------
calendarAPI :: Text
calendarAPI = "https://www.googleapis.com/calendar/v3/calendars/"

--------------------------------------------------------------------------------
newtype GoogleDate = GoogleDate { date :: UTCTime } deriving (Show, Eq)

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

data CalendarEvent = CalendarEvent {
        ce_id :: Text
      , ce_hangoutLink :: Text
      , ce_iCalUID :: Text
      , ce_htmlLink :: Text
      } deriving (Show, Eq)


deriveFromJSON defaultOptions { labelFieldModifier = drop 3 } ''CalendarEvent

--------------------------------------------------------------------------------
-- | Insert a new event inside the given calendar.
newEvent :: CalendarId -> CalendarEventReq -> IO (Either String CalendarEvent)
newEvent cId cev@CalendarEventReq{..}= do
  rq' <- parseUrl $ T.unpack $ calendarAPI <> "/events"
  let headers = [(hContentType, "application/json")
               ,(hUserAgent, "memento/0.0.0")
               ,(hAuthorization, "Bearer " <> calendarToken)]
  let jsonBody = JSON.encode $ def { message = msg }
  let rq = def {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyBS jsonBody
      , secure = True
      }
  res <- withManager $ httpLbs rq
  return $ JSON.eitherDecode res

--------------------------------------------------------------------------------
newHangoutLink :: Text -> IO (Either String Text)
newHangoutLink title = do
  req <- newCalendarEvtRq title
  newEvent calendarId req >>= either id ce_hangoutLink
