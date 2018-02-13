module B21.Types where

import Data.Aeson
import Data.Monoid ( (<>) )
import Data.Text ( Text, pack )
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Format
import GHC.Generics
import Web.FormUrlEncoded

newtype DateTime
  = DateTime { dateTime :: UTCTime }

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
    , ctsSunday :: Text
    , ctsSaturday :: Text
    , ctsRate :: Double
    , ctsHours :: [Double]
      -- ^ Number of hours worked on Sunday, Monday, Tuesday, Wednesday,
      -- Thursday, Friday, Saturday.
    }
  deriving (Eq, Ord, Generic, FromJSON)

instance FromForm CreateTimesheet where
  fromForm f = CreateTimesheet
    <$> parseUnique "name" f
    <*> parseUnique "id" f
    <*> pure "Office of Student Life and Learning"
    <*> (datify <$> yearsFrom <*> monthsFrom <*> daysFrom)
    <*> (datify <$> yearsTo <*> monthsTo <*> daysTo)
    <*> parseUnique "rate" f
    <*> hours
    where
      daysFrom = parseUnique "days_from" f
      monthsFrom = parseUnique "months_from" f
      yearsFrom = parseUnique "years_from" f
      daysTo = parseUnique "days_to" f
      monthsTo = parseUnique "months_to" f
      yearsTo = parseUnique "years_to" f

      days = ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]
      hours = traverse prs days where
        prs d = parseUnique d f

      datify :: Text -> Text -> Text -> Text
      datify y m d = y <> "-" <> m <> "-" <> d

data TimesheetInfo
  = TimesheetInfo
    { timesheetUrl :: String
    }
  deriving (Generic, ToJSON)
