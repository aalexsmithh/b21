module Timesheet (makeTimesheet) where

import B21.Types ( CreateTimesheet(..), TimesheetInfo(..) )

import qualified Data.Text as T
import System.Exit ( ExitCode(ExitSuccess) )
import System.Process
import System.Timeout
import System.IO ( FilePath )

-- | Generates a timesheet using the script at the given path and environment.
-- Returns the filepath if the timesheet was successfully generate
makeTimesheet
  :: FilePath -- ^ output file path
  -> FilePath -- ^ script path
  -> FilePath -- ^ timesheet base path
  -> Maybe [(String, String)] -- ^ existing environment
  -> CreateTimesheet -- ^ timesheet parameters
  -> IO Bool
makeTimesheet outPath scriptPath tsbase env cts = do
  (_, _, _, h) <- createProcess $ (proc scriptPath [])
    { env = (++) <$> env <*> pure env' }
  timeout' (waitForProcess h) >>= \case
    Just ExitSuccess -> pure True
    _ -> pure False
  where
    env' = ("TS_BASE", tsbase) : ("OUTNAME", outPath) : makeEnv cts

-- | Creates an environment mapping to send all necessary parameters to the
-- script.
-- Note: the mapping does not set OUTNAME which is where the script writes its
-- output.
makeEnv :: CreateTimesheet -> [(String, String)]
makeEnv CreateTimesheet{..} =
  [ ("TS_NAME", T.unpack ctsName)
  , ("TS_ID", T.unpack ctsId)
  , ("TS_DEPT", T.unpack ctsDept)
  , ("TS_SUNDAY", T.unpack ctsSunday)
  , ("TS_SATURDAY", T.unpack ctsSaturday)
  , ("TS_RATE", show ctsRate)
  ] ++ days where
    days :: [(String, String)]
    days = zipWith f w ctsHours
    f :: String -> Double -> (String, String)
    f day h = ("TS_TOT_" ++ day, show h)
    w :: [String]
    w = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"]

-- | Timeout duration, in microseconds.
timeoutSec = 20 * 1000000
timeout' = timeout timeoutSec
