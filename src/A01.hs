module A01 (a01_input, a01_ans1, a01_ans2) where

import Data.Ratio ((%))

inputFile :: String
inputFile = "data/a01/input.txt"

a01_input :: IO [Int]
a01_input = fmap (fmap read . lines) $ readFile inputFile

fuel :: Int -> Int
fuel m
    | f < 0 = 0
    | otherwise = f
  where
    f = floor (m % 3) - 2


fuelRec :: Int -> Int
fuelRec m
    | f' == 0 = f
    | otherwise = f + fuelRec f
  where
    f = fuel m
    f' = fuel f

a01_ans1 :: [Int] -> Int
a01_ans1 = sum . fmap fuel

a01_ans2 :: [Int] -> Int
a01_ans2 = sum . fmap fuelRec
