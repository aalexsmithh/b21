module B21.Types where

import Data.Aeson
import Data.Bifunctor
import Data.Fixed ( Fixed(MkFixed) )
import Data.Monoid ( (<>) )
import Data.Ratio ( (%), numerator )
import Data.Text ( Text, pack )
import Data.Time.Calendar (addDays,  Day )
import Data.Time.Clock ( UTCTime(..), diffUTCTime )
import Data.Time.LocalTime ( TimeOfDay, LocalTime(..), localTimeToUTC, utc )
import Data.Time.Format
import GHC.Generics
import Web.FormUrlEncoded
import Web.HttpApiData ( FromHttpApiData(..) )

newtype DateTime
  = DateTime { dateTime :: UTCTime }

-- | A pair of times, representing when the employee clocked in and
-- out.
data WorkHours =
  WorkHours
  { workHoursIn :: LocalTime
  , workHoursOut :: LocalTime
  }
  deriving (Eq, Ord)

instance FromJSON WorkHours where
  parseJSON (Object o) =
    WorkHours <$> o .: "timeIn" <*> o .: "timeOut"

instance ToJSON WorkHours where
  toJSON WorkHours{..} = object
    [ "timeIn" .= workHoursIn
    , "timeOut" .= workHoursOut
    ]

-- | Computes the number of minutes worked.
workMinutes :: WorkHours -> Integer
workMinutes WorkHours{..} = round d `div` 60 where
  d = diffUTCTime (localTimeToUTC utc workHoursOut) (localTimeToUTC utc workHoursIn)
    -- d has resolution of picoseconds, i.e. 10^-12 s, but is treated
    -- as *seconds* by conversion functions, e.g. `round`.
    -- so to get minutes, it suffices to divide the rounding by 60.

data AddEmail
  = AddEmail
    { addEmailAddress :: Text
    }
    deriving (Generic, FromJSON, ToJSON)

data Event
  = Event
    { eventStart :: DateTime
    , eventEnd :: DateTime
    , eventTitle :: Text
    , eventSummary :: Text
    }

instance ToJSON Event where
  toJSON Event{..} = object
    [ "title" .= eventTitle
    , "summary" .= eventSummary
    , "startDate" .= eventStart
    , "endDate" .= eventEnd
    ]

iso8601 = iso8601DateFormat (Just "%H:%M:%SZ")

instance ToJSON DateTime where
  toJSON (DateTime t) = String $ pack (formatTime defaultTimeLocale iso8601 t)

data CreateTimesheet
  = CreateTimesheet
    { ctsName :: Text
    , ctsId :: Text
    , ctsDept :: Text
    , ctsSunday :: Day
    , ctsSaturday :: Day
    , ctsRate :: Double
    , ctsHours :: [Maybe WorkHours]
      -- ^ Number of hours worked on Sunday, Monday, Tuesday, Wednesday,
      -- Thursday, Friday, Saturday.
    }
  deriving (Eq, Ord, Generic, FromJSON)

instance FromForm CreateTimesheet where
  fromForm f = do
    sunday <- bind3 datify yearsFrom monthsFrom daysFrom
    saturday <- bind3 datify yearsTo monthsTo daysTo

    CreateTimesheet
      <$> parseUnique "name" f
      <*> parseUnique "id" f
      <*> parseUnique "department" f
      <*> pure sunday
      <*> pure saturday
      <*> parseUnique "rate" f
      <*> hours sunday
    where
      days = ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]
      daysFrom = parseUnique "days_from" f
      monthsFrom = parseUnique "months_from" f
      yearsFrom = parseUnique "years_from" f
      daysTo = parseUnique "days_to" f
      monthsTo = parseUnique "months_to" f
      yearsTo = parseUnique "years_to" f

      hours sunday = traverse prs (enumerate days) where
        prs (i, d) = do
          let withDay = LocalTime (addDays i sunday)
          let lookupAndParseTime suffix = do
                case lookupUnique (d <> suffix) f of
                  Left _ -> Right Nothing
                  Right "" -> Right Nothing
                  Right s -> Just <$> parseQueryParam s
          inTime <- lookupAndParseTime "_in"
          outTime <- lookupAndParseTime "_out"
          pure $ WorkHours <$> (withDay <$> inTime) <*> (withDay <$> outTime)

      datify :: Text -> Text -> Text -> Either Text Day
      datify y m d = const ("parsing day " <> s) `first` parseQueryParam s where
        s = y <> "-" <> m <> "-" <> d

      bind3 f mx my mz = do
        x <- mx
        y <- my
        z <- mz
        f x y z

      enumerate = zip [0..]

data TimesheetInfo
  = TimesheetInfo
    { timesheetUrl :: String
    }
  deriving (Generic, ToJSON)
