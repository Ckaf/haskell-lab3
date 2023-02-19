module Approximation
  ( segmentApproximation,
    linearApproximation,
    CalculatedPoints (Linear, xPoints, yPoints, Segment),
    Interval,
    Points,
  )
where

import Data.List (elemIndex)
import Data.Maybe

type Interval = (Double, Double)

type Points = [(Double, Double)]

calcLinearCoefficients :: [Double] -> [Double] -> [Double] -> [Double] -> (Double, Double)
calcLinearCoefficients xList xxList yList xyList =
  let lengthDouble = fromIntegral $ length xList
      sx = sum xList
      sxx = sum xxList
      sy = sum yList
      sxy = sum xyList

      delta = sxx * lengthDouble - sx * sx
      delta1 = sxy * lengthDouble - sx * sy
      delta2 = sxx * sy - sx * sxy

      aC = delta1 / delta
      bC = delta2 / delta
   in (aC, bC)

data CalculatedPoints
  = Linear {xPoints :: [Maybe Double], yPoints :: [Maybe Double]}
  | Segment {xPoints :: [Maybe Double], yPoints :: [Maybe Double]}
  deriving (Show, Eq)

calculatedX :: Double -> Double -> Double -> [Maybe Double]
calculatedX left right step =
  let generated = [left, left + step .. right]
      output = map Just generated
   in output

toLower :: Double -> [Double] -> Maybe Double
toLower _ [] = Nothing
toLower val (x : xs) = if val > x && isNothing (toLower val xs) then Just x else toLower val xs

toUpper :: Double -> [Double] -> Maybe Double
toUpper _ [] = Nothing
toUpper val (x : xs) = if x >= val then Just x else toUpper val xs

linearApproximation :: Interval -> Double -> Points -> CalculatedPoints
linearApproximation interval step list =
  let left = fst interval
      right = snd interval
      xList = getXList list
      yList = getYList list
      xxList = map (\x -> x * x) xList
      xyList = zipWith (*) xList yList

      coefficients = calcLinearCoefficients xList xxList yList xyList
      generatedX = calculatedX left right step
      generatedY = map (\mx -> mx >>= \x -> return $ fst coefficients * x + snd coefficients) generatedX
   in Linear generatedX generatedY

calcForSegment :: Double -> Double -> Double -> [Double] -> [Double] -> Maybe Double
calcForSegment xGiven xLower xUpper xList yList =
  let yLower = yList !! fromJust (elemIndex xLower xList)
      yUpper = yList !! fromJust (elemIndex xUpper xList)
   in Just ((xGiven - xLower) * (yUpper - yLower) / (xUpper - xLower) + yLower)

segmentApproximation :: Interval -> Double -> Points -> CalculatedPoints
segmentApproximation interval step list =
  let left = fst interval
      right = snd interval
      xList = getXList list
      yList = getYList list
      generatedX = calculatedX left right step
      generatedY = map (\mx -> mx >>= \x -> toLower x xList >>= \y -> toUpper x xList >>= \z -> calcForSegment x y z xList yList) generatedX
   in Segment generatedX generatedY

getXList :: Points -> [Double]
getXList = map fst

getYList :: Points -> [Double]
getYList = map snd
