module Approximation(
segmentApproximation,
linearApproximation,
CalculatedPoints, Interval, Points
) where



import Data.List
import Data.Maybe

type Xlist = [Double]
type Ylist = [Double]
type Interval = (Double, Double)
type Points = [(Double, Double)] 

calcLinearCoefficients :: Int -> Xlist -> [Double] -> Ylist -> [Double] -> (Double, Double)
calcLinearCoefficients length xList xxList yList xyList =
    let
        lengthDouble = fromIntegral length
        sx = sum xList
        sxx = sum xxList
        sy = sum yList
        sxy = sum xyList

        delta = sxx * lengthDouble - sx * sx
        delta1 = sxy * lengthDouble - sx * sy
        delta2 = sxx * sy - sx * sxy

        aC = delta1 / delta
        bC = delta2 / delta
    in
        (aC,bC)


data CalculatedPoints =
    Linear {a :: Double, b :: Double, xPoints :: [Maybe Double], yPoints :: [Maybe Double] }
    | Segment {xPoints :: [Maybe Double], yPoints :: [Maybe Double]}
    deriving Show


calculatedX :: Double -> Double -> Double -> [Maybe Double]
calculatedX left right step =
    let
        generated = [left, left + step .. right]
        output = map Just generated
    in
        output

toLower :: Double -> [Double] -> Maybe Double
toLower val [] = Nothing
toLower val (x:xs) = if val > x && isNothing (toLower val xs) then Just x else toLower val xs

toUpper :: Double -> [Double] -> Maybe Double
toUpper val [] = Nothing
toUpper val (x:xs) = if x >= val then Just x else toUpper val xs

linearApproximation :: Interval -> Double -> Points -> CalculatedPoints
linearApproximation interval step list =
    let
        left = fst interval
        right = snd interval
        len = length list
        xList = x_list list
        yList = y_list list
        xxList = map (\x -> x*x) xList
        xyList = zipWith (*) xList yList

        coefficients = calcLinearCoefficients len xList xxList yList xyList
        generatedX = calculatedX left right step
        generatedY = map (\mx -> mx >>= \x -> return $ fst coefficients * x + snd coefficients) generatedX
    in
        uncurry Linear coefficients generatedX generatedY

calcForSegment :: Double -> Double -> Double -> Xlist -> Ylist -> Maybe Double
calcForSegment xGiven xLower xUpper xList yList =
    let
        yLower = yList !! fromJust (elemIndex xLower xList)
        yUpper = yList !! fromJust (elemIndex xUpper xList)

    in
        Just ((xGiven - xLower) * (yUpper - yLower) / (xUpper - xLower) + yLower)

segmentApproximation :: Interval -> Double -> Points -> CalculatedPoints
segmentApproximation interval step list =
    let
        left = fst interval
        right = snd interval
        xList = x_list list
        yList = y_list list
        generatedX = calculatedX left right step
        generatedY = map (\mx -> mx >>= \x -> toLower x xList >>= \y -> toUpper x xList >>= \z ->calcForSegment x y z xList yList) generatedX
    in
        Segment generatedX generatedY

x_list:: Points-> [Double]
x_list = map fst

y_list:: Points -> [Double]
y_list = map snd
