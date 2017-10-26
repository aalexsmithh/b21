module B21.Types where

import Data.Aeson
import Data.Text ( Text, pack )
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Format
import GHC.Generics

newtype DateTime
  = DateTime { dateTime :: UTCTime }

data AddEmail
  = AddEmail
    { addEmailAddress :: Text
    }
    deriving (Generic, FromJSON, ToJSON)

data Event
  = Event
    { eventDate :: DateTime
    , eventStart :: DateTime
    , eventEnd :: DateTime
    , eventInfo :: Text
    }

instance ToJSON Event where
  toJSON Event{..} = object
    [ "date" .= eventDate
    , "and" .= eventInfo
    ]

iso8601 = iso8601DateFormat (Just "%H:%M:%SZ")

instance ToJSON DateTime where
  toJSON (DateTime t) = String $ pack (formatTime defaultTimeLocale iso8601 t)
