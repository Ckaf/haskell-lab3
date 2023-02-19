{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Approximation
import Data.List.Split
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.Console.CmdArgs
import System.IO

data Options = Options
  { left :: Double,
    right :: Double,
    file :: String,
    lm :: Bool,
    sm :: Bool
  }
  deriving (Show, Data, Typeable)

inputOptions :: Options
inputOptions =
  Options
    { file = "" &= help "File path with points",
      left = def &= help "Left border",
      right = def &= help "Right border",
      lm = False &= help "Linear Method",
      sm = False &= help "Segment Method"
    }

calculatePoints :: Interval -> Double -> Points -> Bool -> Bool -> [Maybe CalculatedPoints]
calculatePoints interval step points True True =
  [ Just (linearApproximation interval step points),
    Just (segmentApproximation interval step points)
  ]
calculatePoints interval step points True False = [Just (linearApproximation interval step points), Nothing]
calculatePoints interval step points False True = [Nothing, Just (segmentApproximation interval step points)]
calculatePoints _ _ _ False False = [Nothing, Nothing]

printResult :: [Maybe CalculatedPoints] -> IO ()
printResult [lpoints, Nothing] = do
  putStrLn "Linear Aproxiation"
  let points = fromMaybe (Linear [] []) lpoints
  prettyPoints (xPoints points) (yPoints points)
printResult [Nothing, spoints] = do
  putStrLn "Segment Aproxiation"
  let points = fromMaybe (Linear [] []) spoints
  prettyPoints (xPoints points) (yPoints points)
printResult [lpoints, spoints] = do
  putStrLn "Linear Aproxiation"
  let points1 = fromMaybe (Linear [] []) lpoints
  prettyPoints (xPoints points1) (yPoints points1)
  putStrLn "Segment Aproxiation"
  let points2 = fromMaybe (Linear [] []) spoints
  prettyPoints (xPoints points2) (yPoints points2)
printResult _ = putStrLn "Calculation failed. Perhaps you did not specify a method."

main :: IO ()
main = do
  input <- cmdArgs inputOptions
  case file input of
    "" -> noFileCalculate input []
    path -> do
      p <- getPointsFile path
      let r = calculatePoints (left input, right input) 0.95 p (lm input) (sm input)
      printResult r

getPointsFile :: String -> IO Points
getPointsFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let points_str = lines contents
  let points = foldl (\l d -> l ++ [tuplify2 $ splitOn ";" d]) [] points_str
  return points

getPointsNoFile :: Points -> IO Points
getPointsNoFile xs = do
  -- getting user input
  print "Enter a point in the format: x;y"
  end <- isEOF
  case end of
    True -> return []
    False -> do
      input <- getLine
      if input == "exit"
        then return []
        else return $ xs ++ [tuplify2 $ splitOn ";" input]

noFileCalculate :: Options -> Points -> IO ()
noFileCalculate ops ps = do
  new_ps <- getPointsNoFile ps
  case new_ps of
    [] -> return ()
    _ -> do
      let r = calculatePoints (left ops, right ops) 0.95 new_ps (lm ops) (sm ops)
      printResult r
      noFileCalculate ops new_ps

tuplify2 :: [String] -> (Double, Double)
tuplify2 [x, y] = (read x :: Double, read y :: Double)
tuplify2 _ = error "tuplify2 error"

prettyPoints :: [Maybe Double] -> [Maybe Double] -> IO ()
prettyPoints [] _ = do
  putStrLn ""
prettyPoints xL yL = do
  putStr "x: "
  if isJust (head xL) then putStr $ show (fromJust (head xL)) else putStr "undefined"
  putStr " y: "
  maybe (print "undefined") print $ head yL
  prettyPoints (tail xL) (tail yL)
