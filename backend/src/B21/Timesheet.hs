module B21.Timesheet (makeTimesheet) where

import B21.Types (WorkHours(..), workMinutes,  CreateTimesheet(..), TimesheetInfo(..) )

import Data.Time.Format (defaultTimeLocale,  formatTime )
import qualified Data.Text as T
import System.Exit ( ExitCode(ExitSuccess) )
import System.Process
import System.Timeout

-- | Generates a timesheet using the script at the given path and environment.
-- The boolean represents whether the subprocess completed successfully.
makeTimesheet
  :: FilePath -- ^ output file path
  -> FilePath -- ^ script path
  -> FilePath -- ^ timesheet base path
  -> Maybe [(String, String)] -- ^ existing environment
  -> CreateTimesheet -- ^ timesheet parameters
  -> IO Bool
makeTimesheet outPath scriptPath tsbase origEnv cts = do
  let env = (++) <$> origEnv <*> pure env'
  putStrLn $ "Generating timesheet with parameters " ++ show env
  (_, _, _, h) <- createProcess $ (proc scriptPath [])
    { env = env }
  putStrLn "waiting for subprocess..."
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
  , ("TS_SUNDAY", formatDay ctsSunday)
  , ("TS_SATURDAY", formatDay ctsSaturday)
  , ("TS_RATE", show ctsRate)
  ] ++ concat days where
    days = zipWith f w (ctsHours)
    f :: String -> Maybe WorkHours -> [(String, String)]
    f day Nothing =
      [ ("TS_TOT_" ++ day, "0")
      , ("TS_IN_" ++ day, "")
      , ("TS_OUT_" ++ day, "")
      ]
    f day (Just h@WorkHours{..}) =
      [ ("TS_TOT_" ++ day, show n)
      , ("TS_IN_" ++ day, formatTod workHoursIn)
      , ("TS_OUT_" ++ day, formatTod workHoursOut)
      ]
      where
        -- compute the hours worked from the minutes worked
        n = round (fromIntegral (workMinutes h) / 60 :: Double) :: Integer

    -- format a 'Day' into YYYY-MM-DD
    formatDay d = formatTime defaultTimeLocale "%F" d
    -- format a 'TimeOfDay' into HH:MM
    formatTod = formatTime defaultTimeLocale "%H:%M"
    -- day-of-week codes used by timesheet.sh
    w = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"]

-- | Timeout duration, in microseconds.
timeoutSec = 20 * 1000000
timeout' = timeout timeoutSec
