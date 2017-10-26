module Main where

import B21.Api
import B21.Types

import Calendar

import Control.Concurrent.Async ( async, waitCatch )
import Control.Concurrent.MVar
import Control.Exception ( SomeException )
import Control.Monad ( forM_ )
import Control.Monad.IO.Class
import Data.Text ( Text )
import qualified Data.Text.IO as T
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.HTTP.Client ( parseRequest_, Request, Manager, newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Servant
import System.IO ( FilePath )
import System.Environment ( getEnv )

type Lock = MVar ()

data SiteConfig =
  SiteConfig
    { confEmailFilePath :: FilePath
    , confServerPort :: Int
    , confFileLock :: Lock
    , confCalendarReq :: Request
    , confHttpManager :: Manager
    }

-- | Await a lock, perform an action, and restore the lock.
-- This operation is exception-safe; if the action raises an exception, the
-- lock is released.
locking :: Lock -> IO a -> IO a
locking l m = withMVar l (const m)

getConfig :: IO SiteConfig
getConfig = do
  port <- read <$> getEnv "B21_API_PORT"
  path <- getEnv "B21_EMAIL_FILEPATH"
  calReq <- parseRequest_ <$> getEnv "B21_CALENDAR_URI"
  lock <- newMVar ()
  man <- newManager tlsManagerSettings
  pure SiteConfig
    { confEmailFilePath = path
    , confServerPort = port
    , confFileLock = lock
    , confCalendarReq = calReq
    , confHttpManager = man
    }

main :: IO ()
main = do
  conf <- getConfig
  run (confServerPort conf) $ logStdoutDev $ serve b21Api $ server conf

catchAll :: IO a -> IO (Either SomeException a)
catchAll m = async m >>= waitCatch

server :: SiteConfig -> Server B21Api
server SiteConfig{..} = addEmail :<|> getEvents where
  addEmail :: AddEmail -> Handler Text
  addEmail AddEmail{..} = do
    liftIO (catchAll $ addEmail' addEmailAddress) >>= \case
      Left _ -> throwError err500 { errBody = "something happened" }
      Right _ -> pure "ok"

  getEvents :: Handler [Event]
  getEvents = liftIO $ do
    let r (ers, evs) (ers', evs') = (ers ++ ers', evs ++ evs')
    (ers, evs) <- foldr r ([], []) . fmap events . fst
      <$> getRemoteCalendar confCalendarReq confHttpManager

    forM_ ers $ \e -> putStrLn $ "error interpreting event: " ++ e

    pure evs

  addEmail' :: Text -> IO ()
  addEmail' x = locking confFileLock (T.appendFile confEmailFilePath x)
