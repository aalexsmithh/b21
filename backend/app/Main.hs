module Main where

import B21.Api
import B21.Types

import Control.Concurrent.Async ( async, waitCatch )
import Control.Concurrent.MVar
import Control.Exception ( SomeException )
import Control.Monad.IO.Class
import Data.Text ( Text )
import qualified Data.Text.IO as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO ( FilePath )
import System.Environment ( getEnv )

type Lock = MVar ()

data SiteConfig =
  SiteConfig
    { confEmailFilePath :: FilePath
    , confServerPort :: Int
    , confFileLock :: Lock
    }

-- | Await a lock, perform an action, and restore the lock.
-- This operation is exception-safe; if the action raises an exception, the
-- lock is released.
locking :: Lock -> IO a -> IO a
locking l m = withMVar l (const m)

getConfig :: IO SiteConfig
getConfig = do
  port <- read <$> getEnv "B21_PORT"
  path <- getEnv "B21_EMAIL_FILEPATH"
  lock <- newMVar ()
  pure SiteConfig
    { confEmailFilePath = path
    , confServerPort = port
    , confFileLock = lock
    }

main :: IO ()
main = do
  conf <- getConfig
  run (confServerPort conf) $ serve b21Api $ server conf

catchAll :: IO a -> IO (Either SomeException a)
catchAll m = async m >>= waitCatch

server :: SiteConfig -> Server B21Api
server SiteConfig{..} = addEmail where
  addEmail :: AddEmail -> Handler Text
  addEmail AddEmail{..} = do
    liftIO (catchAll $ addEmail' addEmailAddress) >>= \case
      Left _ -> throwError err500 { errBody = "something happened" }
      Right _ -> pure "ok"

  addEmail' :: Text -> IO ()
  addEmail' x = locking confFileLock (T.appendFile confEmailFilePath x)
