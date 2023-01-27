import Approximation
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

test1 :: Test
test1 = TestCase (assertEqual "Test linearApproximation" expectedResult (linearApproximation (1, 10) 0.95 points))
  where
    points = [(1, 2), (2, 3)]
    expectedResult = Linear [Just 1.0,Just 1.95,Just 2.9,Just 3.8499999999999996,Just 4.8,Just 5.75,Just 6.699999999999999,Just 7.6499999999999995,Just 8.6,Just 9.549999999999999]
     [Just 2.0,Just 2.95,Just 3.9,Just 4.85,Just 5.8,Just 6.75,Just 7.699999999999999,Just 8.649999999999999,Just 9.6,Just 10.549999999999999]

test2 :: Test
test2 = TestCase (assertEqual "Test segmentApproximation" expectedResult (segmentApproximation (1, 3) 0.95 points))
  where
    points = [(1, 2), (2, 3)]
    expectedResult = Segment [Just 1.0, Just 1.95, Just 2.9] [Nothing, Just 2.95, Nothing]

tests :: Test
tests = TestList [test1, test2]
