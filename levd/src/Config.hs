-- Config.hs
-- Functions / data types for working with the levd.cfg config file

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

--import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import Data.Yaml
import Data.List
import Control.Applicative -- <$>, <*>
import Data.Map (Map)
import qualified Data.Map as Map

import Constants
import Common

-- Pseduoym for a 2d point in a grid
type Point = (Double, Double)

-- Type represeting function (y = Mx + b)
type Slope = Double
type Shift = Double

-- Type representing collection of Ranges and corresponding mapping fn to pump val
type OrderedLines = (Map Double Line)

-- Represents y = mx + b
data Line = Line Slope Shift deriving (Show, Generic)

-- Function that returns y val from expected x
-- Used to return fan speed (y val) from temperature (x val)
yVal :: Line -> Double -> Double
yVal (Line slope shift) x = (slope * x) + shift

-- Function used to create 'Line' AST
calcSlope :: Point -> Point -> Slope
calcSlope a b = (snd b - snd a) / (fst b - fst a)

-- Function used to calculate b value in y = mx + b
calcShift :: Point -> Slope -> Shift
calcShift p slope = snd p - (slope * (fst p))

-- Returns a Line from two points
createLine :: Point -> Point -> Line
createLine a b =
  let slope = calcSlope a b
  in Line slope (calcShift a slope)

-- TODO: Understand lowerbound more
-- Take two points and make them a line, continue until end of list
sortedPointsToLine :: [Point] -> [(Double, Line)]
sortedPointsToLine [] = []
sortedPointsToLine (x : []) = []
sortedPointsToLine (x : xs) = (fst next, (createLine x next)) : (sortedPointsToLine xs)
  where next = head xs

-- ===== Public interface =====
--
-- Takes data points read from YAML file and creates the OrderedLines type
-- Also inserts very first element
linesByRanges :: [Point] -> OrderedLines
linesByRanges xs = Map.fromList (sortedPointsToLine (sortTupleListByFirst (withSentinal xs)))
  where withSentinal (num, _) : _ = num
          | num < 0 = 0
          | num == 0 = 0
          | otherwise = 0
          

-- AST Representing the parameters in a levd.cfg config
data LevdConfigParse
  = LevdConfigParse {
    fan_profile  :: [(Double, Double)],
    main_color   :: Int,
    alt_color    :: Int,
    enable_color :: Bool,
    enable_blink :: Bool,
    blink_ival   :: Int,
    alter_ival   :: Int
} deriving (Show)

instance FromJSON Line
instance FromJSON LevdConfigParse where
    parseJSON (Object v) = LevdConfigParse <$>
                           v .: "fan_profile" <*>
                           v .:? "main_color" .!= krakenDefaultColor <*>
                           v .:? "alt_color"  .!= krakenDefaultColor <*>
                           v .: "enable_color" <*>
                           v .: "enable_blink" <*>
                           v .: "blink_ival" <*>
                           v .: "alter_ival"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse LevdConfig from YAML/JSON"

-- AST Representing condensed version of config file
type MainColor     = Int
type AltColor      = Int
type BlinkInterval = Int
type AltInterval   = Int
data LevdColor =
  LevdPlain MainColor |
  LevdBlink MainColor BlinkInterval |
  LevdBlinkChg MainColor AltColor BlinkInterval |
  LevdChgColor MainColor AltColor AltInterval deriving (Show)

data LevdCfg
  = LevdCfg {
    fan_pro :: OrderedLines,
    color_opts  :: LevdColor
} deriving (Show)

condenseConfig :: LevdConfigParse -> LevdCfg
condenseConfig lvdp
  | colorOn && blinkOn = LevdCfg fpro (LevdBlinkChg 0 0 0)
  | colorOn && blinkOn == False = LevdCfg fpro (LevdChgColor 0 0 0)
  | colorOn == False && blinkOn = LevdCfg fpro (LevdBlink 0 0)
  | otherwise = LevdCfg fpro (LevdPlain 0)
  where fpro = linesByRanges (fan_profile lvdp)
        colorOn = (enable_color lvdp)
        blinkOn = (enable_blink lvdp)



-- 1.
-- fromList [(30.0,Line 2.0 (-30.0)),(35.0,Line 4.0 (-100.0)),(40.0,Line 5.0 (-140.0)),(42.0,Line 10.0 (-350.0)),(43.0,Line 10.0 (-350.0))]

-- 2.
-- fromList [(35.0,Line 2.0 (-30.0)),(40.0,Line 4.0 (-100.0)),(42.0,Line 5.0 (-140.0)),(43.0,Line 10.0 (-350.0)),(45.0,Line 10.0 (-350.0))]
