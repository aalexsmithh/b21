module B21.Types where

import Data.Aeson
import Data.Text ( Text )
import GHC.Generics

data AddEmail
  = AddEmail
    { addEmailAddress :: Text
    }
    deriving (Generic, FromJSON, ToJSON)
