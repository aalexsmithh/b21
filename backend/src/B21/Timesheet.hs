module B21.Timesheet (makeTimesheet) where

import B21.Types ( CreateTimesheet(..), TimesheetInfo(..) )

import System.Process
import System.Timeout

-- | Generates a timesheet using the script at the given path and environment.
-- The boolean represents whether the subprocess completed successfully.
makeTimesheet
  :: String -- ^ output file path
  -> String -- ^ script path
  -> Maybe [(String, String)] -- ^ existing environment
  -> CreateTimesheet -- ^ timesheet parameters
  -> IO Bool
makeTimesheet outPath scriptPath env cts = do
  (_, _, _, h) <- createProcess $ proc path []
    { env = (++) <$> env <*> pure (("OUTNAME", outPath) : makeEnv cts) }
  timeout' (waitForProcess h) >>= \case
    Just ExitSuccess -> pure True
    _ -> pure False

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
    days = zipWith f w (adjustHours <$> ctsHours)
    f (day, h) = ("TS_TOT_" ++ day, show h)
    w = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"]
    adjustHours h = if null h then 0 else h

-- | Timeout duration, in microseconds.
timeoutSec = 20 * 1000000
timeout' = timeout timeoutSec
