module B21.Api
( B21Api
, b21Api
) where

import Data.Proxy
import Data.Text ( Text )
import GHC.Generics
import Servant.API

import B21.Types

type B21Api
  = "timesheet"
    :> ReqBody '[FormUrlEncoded] CreateTimesheet
    :> Post '[JSON] ()

b21Api :: Proxy B21Api
b21Api = Proxy
