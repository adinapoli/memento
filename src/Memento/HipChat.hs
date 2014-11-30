
module Memento.HipChat where

import Data.Text (Text)
import Data.Default
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Aeson as JSON

-- | Simple HipChat notifier module for Memento.


--------------------------------------------------------------------------------
-- SUPER TEMPORARY - DO not commit, put inside a cfg file.
apiToken :: Text
apiToken = "cf8192c7ae82fd06a82c7e9f121145"

--------------------------------------------------------------------------------
data HCRoom =
      Dev
    | Releases deriving (Show, Eq)

--------------------------------------------------------------------------------
data HCPayload = HCPayload {
        color :: HCColour
      , message :: Text
      , notify :: Boolean
      }

data HCColour = HC_Purple deriving (Show, Eq)


instance Default HCPayload where
    def = HCPayload HC_Purple mempty True


--------------------------------------------------------------------------------
renderRoom :: HCRoom -> Text
renderRoom Dev = "Development"
renderRoom Releases = "Releases"

--------------------------------------------------------------------------------
apiUrl :: Text
apiUrl = "https://api.hipchat.com"

--------------------------------------------------------------------------------
-- | Notify something inside HipChat
notify :: HCRoom -> Text -> IO ()
notify room msg = do
  rq' <- parseUrl $ T.unpack $ apiUrl <> "/v2/room/" <> renderRoom room <> "/notification"
  let headers = [(hContentType, "application/json")
              ,(hUserAgent, "memento/0.0.0")
              ,(hAuthorization, "Bearer " <> apiToken)]
  let jsonBody = JSON.encode $ def { message = msg }
  let rq = def {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyBS jsonBody
      , secure = True
      }
  void $ withManager $ httpLbs rq

--------------------------------------------------------------------------------
updateStatus :: HCStatus -> IO ()
updateStatus _ = _
