import Approximation
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

truncate' :: Maybe Double -> Int -> Maybe Double
truncate' Nothing _ = Nothing
truncate' (Just x) n = Just $ fromIntegral (round (x * t):: Integer) / t
  where
    t = 10 ^ n

precRoundArray :: [Maybe Double] -> Int -> [Maybe Double]
precRoundArray list prec = map (`truncate'` prec) list

precRound :: CalculatedPoints -> Int -> CalculatedPoints
precRound (Linear x y) prec = Linear (precRoundArray x prec) (precRoundArray y prec)
precRound (Segment x y) prec = Segment (precRoundArray x prec) (precRoundArray y prec)

test1 :: Test
test1 = TestCase (assertEqual "Test linearApproximation" expectedResult actualResult)
  where
    points = [(1, 2), (2, 3)]
    expectedResult =
      Linear
        [Just 1.0, Just 1.95, Just 2.9, Just 3.85, Just 4.8, Just 5.75, Just 6.7, Just 7.65, Just 8.6, Just 9.55]
        [Just 2.0, Just 2.95, Just 3.9, Just 4.85, Just 5.8, Just 6.75, Just 7.7, Just 8.65, Just 9.6, Just 10.55]
    actualResult = precRound (linearApproximation (1, 10) 0.95 points) 2

test2 :: Test
test2 = TestCase (assertEqual "Test segmentApproximation" expectedResult (segmentApproximation (1, 3) 0.95 points))
  where
    points = [(1, 2), (2, 3)]
    expectedResult = Segment [Just 1.0, Just 1.95, Just 2.9] [Nothing, Just 2.95, Nothing]

tests :: Test
tests = TestList [test1, test2]
