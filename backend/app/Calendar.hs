module Calendar where

import B21.Types as B21

import Control.Exception
import Data.Default.Class ( def )
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( Text, toStrict )
import Data.Typeable ( cast )
import qualified Data.Map as M
import Text.ICalendar.Parser
import Text.ICalendar.Types
  ( VCalendar(..), VEvent(..), Date, DateTime(..), DTStart(..), DTEnd(..)
  , Summary(..), Description(..)
  )
import qualified Text.ICalendar.Types as ICal
import Network.HTTP.Client

data CalendarParseException
  = CalendarParseException String
  deriving Show

instance Exception CalendarParseException where
  toException e = SomeException e
  fromException (SomeException e) = cast e

-- | Sends an HTTP request with the given manager and parses the resulting
-- iCalendar string.
-- If the parse fails, this function throws a 'CalendarParseException'.
getRemoteCalendar
  :: Request -- ^ HTTP request to send
  -> Manager -- ^ connection manager
  -> IO ([VCalendar], [String]) -- ^ parsed calendar and list of warnings
getRemoteCalendar req man = do
  res <- responseBody <$> httpLbs req man
  case parseICalendar def (show $ getUri req) res of
    Left s -> throwIO (CalendarParseException s)
    Right x -> pure x

-- | Extracts all events from a calendar.
events :: VCalendar -> [Event]
events VCalendar{..} = catMaybes $ f <$> M.toList vcEvents where
  f :: ((Text, Maybe (Either Date ICal.DateTime)), VEvent) -> Maybe Event
  f ((uid, Just (Right dt)), VEvent{..}) = do
    dtstart <- veDTStart >>= \case
      DTStartDateTime{..} -> b21dateTime dtStartDateTimeValue
      _ -> Nothing

    dtend <- veDTEndDuration >>= \case
      Left DTEndDateTime{..} -> b21dateTime dtEndDateTimeValue
      Right _ -> Nothing

    summary <- summaryValue <$> veSummary

    desc <- descriptionValue <$> veDescription

    pure Event
      { eventDate = dtstart
      , eventStart = dtstart
      , eventEnd = dtend
      , eventInfo = toStrict summary
      }

  f _ = Nothing

b21dateTime :: ICal.DateTime -> Maybe B21.DateTime
b21dateTime d = case d of
  UTCDateTime utc -> pure (B21.DateTime utc)
  _ -> Nothing
