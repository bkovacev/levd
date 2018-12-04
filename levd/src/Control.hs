-- Control.hs
-- Methods pertaining to the workings of the driver program

module Control where

import Constants
import Config
import Common

-- Main AST we are working with
-- data LevdCfg
--   = LevdCfg {
--     fan_pro :: OrderedLines,
--     color_opts  :: LevdColor
-- } deriving (Show)
-- type OrderedLines = (Map Point Line)


-- nextFanCommand :: Int -> OrderedLines -> String
-- nextFanCommand temp fpro = "asdf" --yVal line
--   -- where line = lowerbound fpro temp

-- -- Main loop
-- runDriver :: OrderedLines -> IO ()
-- runDriver fanProfile = do
--   currentTemp <- readTemperatureFile
--   fanValue <- nextFanCommand currentTemp fanProfile
--   putStrLn(fanValue)

