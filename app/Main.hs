{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Approximation
import Data.List.Split
import System.Console.CmdArgs
import System.IO
import Data.Maybe (isJust, fromJust, fromMaybe)

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


parseInput :: IO [Maybe CalculatedPoints]
parseInput = do
  input <- cmdArgs inputOptions
  points <- case file input of
                  "" -> getPointsNoFile []
                  path -> getPointsFile path
           
  return $ calculatePoints (left input, right input) 0.95 points (lm input) (sm input) 
  
main :: IO ()
main = do
  resultPoints <- parseInput
  
  case resultPoints of
    [Nothing, Nothing] -> putStrLn "Calculation failed. Perhaps you did not specify a method."
    [lpoints, Nothing] -> do
      putStrLn "Linear Aproxiation"
      let points = fromMaybe (Linear [] []) lpoints
      prettyPoints (xPoints points) (yPoints points)
    [Nothing, spoints] -> do
      putStrLn "Segment Aproxiation"
      let points = fromMaybe (Linear [] []) spoints
      prettyPoints (xPoints points) (yPoints points)
    [lpoints, spoints] -> do
      putStrLn "Linear Aproxiation"
      let points1 = fromMaybe (Linear [] []) lpoints
      prettyPoints (xPoints points1) (yPoints points1)
      putStrLn "Segment Aproxiation"
      let points2 = fromMaybe (Linear [] []) spoints
      prettyPoints (xPoints points2) (yPoints points2)

    
getPointsFile :: String -> IO Points
getPointsFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let points_str = lines contents
  let points = foldl (\l d -> l ++ [tuplify2 $ splitOn ";" d]) [] points_str
  return points


getPointsNoFile :: [String] -> IO Points
getPointsNoFile xs = do
  -- getting user input
  print "Enter a point in the format: x;y"
  end <- isEOF
  case end of
    True -> do
      let points = foldl (\l d -> l ++ [tuplify2 $ splitOn ";" d]) [] xs
      return (reverse points)
    False -> do
      input <- getLine
      if input == "exit"
        then do
          let points = foldl (\l d -> l ++ [tuplify2 $ splitOn ";" d]) [] xs
          return (reverse points)
        else getPointsNoFile (input : xs)
    

tuplify2 :: [String] -> (Double, Double)
tuplify2 [x, y] = (read x :: Double, read y :: Double)
tuplify2 _ = error "tuplify2 error"

prettyPoints :: [Maybe Double] -> [Maybe Double] -> IO()
prettyPoints [] _ = do
    putStrLn ""
prettyPoints xL yL = do
    putStr "x: "
    if isJust (head xL) then putStr $ show (fromJust (head xL)) else putStr "undefined"
    putStr " y: "
    maybe (print "undefined") print $ head yL
    prettyPoints  (tail xL) (tail yL)