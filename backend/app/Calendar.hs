module Calendar where

import B21.Types as B21

import Control.Exception
import Data.Default.Class ( def )
import Data.Maybe ( catMaybes, fromMaybe )
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

-- | Forgets what exactly the problem is.
forget :: Either a b -> Maybe b
forget (Right x) = Just x
forget (Left _) = Nothing

toEither :: e -> Maybe a -> Either e a
toEither e = maybe (Left e) Right

catEither :: [Either a b] -> ([a], [b])
catEither [] = ([], [])
catEither (e:es) = case e of
  Left x -> (x:as, bs)
  Right x-> (as, x:bs)
  where
    (as, bs) = catEither es

-- | Extracts all events from a calendar.
events :: VCalendar -> ([String], [Event])
events VCalendar{..} = catEither $ f <$> M.toList vcEvents where
  f :: ((Text, Maybe (Either Date ICal.DateTime)), VEvent) -> Either String Event
  f ((uid, _), VEvent{..}) = do
    dtstart <- toEither "no DTStart" veDTStart >>= \case
      DTStartDateTime{..} -> b21dateTime dtStartDateTimeValue
      _ -> Left "event start is not a datetime"

    dtend <- toEither "no DTEnd" veDTEndDuration >>= \case
      Left DTEndDateTime{..} -> b21dateTime dtEndDateTimeValue
      Right _ -> Left "DTEnd is not a datetime"

    summary <- toEither "no summary" (summaryValue <$> veSummary)

    desc <- toEither "no description" (descriptionValue <$> veDescription)

    pure Event
      { eventStart = dtstart
      , eventEnd = dtend
      , eventTitle = toStrict summary
      , eventSummary = toStrict desc
      }

b21dateTime :: ICal.DateTime -> Either String B21.DateTime
b21dateTime d = case d of
  UTCDateTime utc -> pure (B21.DateTime utc)
  _ -> Left "date is not UTC"
