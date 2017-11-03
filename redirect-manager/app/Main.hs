module Main where

import B21.Redirects

import Control.Exception hiding ( Handler )
import Control.Monad ( guard )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import System.IO ( FilePath )
import System.IO.Error ( isDoesNotExistError )
import System.Environment ( getEnv )

data SiteConfig
  = SiteConfig
    { confPort :: Int
    , confSource :: FilePath
    , confDestination :: FilePath
    }

getConfig :: IO SiteConfig
getConfig =
  pure SiteConfig
    <*> (read <$> getEnv "B21_REDIRECTS_PORT")
    <*> getEnv "B21_REDIRECTS_SRC"
    <*> getEnv "B21_REDIRECTS_DST"

main :: IO ()
main = do
  config <- getConfig
  run (confPort config) (logStdoutDev $ serve b21Redirects (server config))

server :: SiteConfig -> Server B21Redirects
server conf = update where
  update :: Handler T.Text
  update = do
    liftIO parseRedirects' >>= \case
      Left e ->
        throwError err500 { errBody = utf8l $ "the redirects file is invalid: " <> e }
      Right x -> do
        liftIO (writeConfig $ renderRedirects x)
        pure "redirects updated successfully"

  writeConfig :: T.Text -> IO ()
  writeConfig = T.writeFile (confDestination conf)

  parseRedirects' :: IO (Either T.Text Redirects)
  parseRedirects' = do
    let src = confSource conf
    parseRedirects src <$> T.readFile src

utf8l :: T.Text -> LBS.ByteString
utf8l = LBS.fromStrict . T.encodeUtf8
