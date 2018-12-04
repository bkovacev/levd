-- Config.hs
-- Functions / data types for working with the levd.cfg config file

{-# LANGUAGE DeriveGeneric #-}

module Config where

import GHC.Generics
import Data.Yaml
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Pseduoym for a 2d point in a grid
type Point = (Double, Double)

-- Type represeting function (y = Mx + b)
type Slope = Double
type Shift = Double

-- Type representing collection of Ranges and corresponding mapping fn to pump val
type OrderedLines = (Map Point Line)

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

-- Take two points and make them a line, continue until end of list
sortedPointsToLine :: [Point] -> [(Point, Line)]
sortedPointsToLine [] = []
sortedPointsToLine (x : []) = []
sortedPointsToLine (x : xs) = (x, (createLine x next)) : (sortedPointsToLine xs)
  where next = head xs

-- ===== Public interface =====
--
-- Takes data points read from YAML file and creates the OrderedLines type
linesByRanges :: [Point] -> OrderedLines
linesByRanges xs = Map.fromList (sortedPointsToLine xs)


-- AST Representing the parameters in a levd.cfg config
data LevdConfig
  = LevdConfig {
    fan_profile  :: [(Double, Double)],
    main_color   :: Int,
    alt_color    :: Int,
    enable_color :: Bool,
    enable_blink :: Bool,
    blink_ival   :: Int,
    alter_ival   :: Int
} deriving (Show, Generic)

instance FromJSON Line
instance FromJSON LevdConfig
