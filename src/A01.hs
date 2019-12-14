module A01 (a01) where

import Data.Ratio ((%))

inputFile :: String
inputFile = "data/a01/input.txt"

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

a01 :: IO ()
a01 = do
       input <- readFile inputFile
       let masses = fmap read $ lines input
       let fs = fmap fuel masses
       let fs' = fmap fuelRec masses
       print $ sum fs
       print $ sum fs'
