{-# LANGUAGE BangPatterns #-}

module A16 (a16_input,a16_ans1,a16_ans2) where

import Data.List (foldl')
import Control.DeepSeq (deepseq)

fileName :: String
fileName = "data/a16/input.txt"

baseSeq :: [Int]
baseSeq = [0,1,0,-1]

repSeq :: Int -> [Int]
repSeq n = concat $ fmap (take n . repeat) baseSeq

factors :: Int -> [Int]
factors n = tail $ concat $ repeat $ repSeq (n+1)

-- get factor for digit of iteration
factor :: Int -> Int -> Int
factor it dig = case r2 of
                  0 -> 0
                  1 -> 1
                  2 -> 0
                  3 -> -1
                  4 -> error ""
  where
    r1 = (dig+1) `rem` (4 * (it+1))
    r2 = r1 `div` (it+1)

step :: [Int] -> [Int]
step is = fmap (\n -> (`rem` 10) $ abs $
                      foldl' (\acc (x,d) -> case factor n d of
                                              0 -> acc
                                              f -> acc + f*x) 0 (zip is [0..])) range
  where
    len = length is
    range = [0..len-1]

steps :: Int -> [Int] -> [Int]
steps 0 xs = xs
steps n xs = xs' `deepseq` steps (n-1) xs'
  where
    xs' = step xs

parseList :: String -> [Int]
parseList = fmap (read . pure) . head . lines

a16_input :: IO [Int]
a16_input = fmap parseList $ readFile fileName

a16_ans1 :: [Int] -> Int
a16_ans1 is = sum $ zipWith (\n x -> x * 10^n) [0..] (reverse first)
  where
    ans = iterate step is !! 100
    first = take 8 ans

a16_ans2 :: [Int] -> Int
a16_ans2 is = sum $ zipWith (\n x -> x * 10^n) [0..] (reverse first)
  where
    is' = concat $ take 10000 $ repeat is
    --ans = iterate step is' !! 100
    ans = steps 100 is'
    first = take 8 ans
