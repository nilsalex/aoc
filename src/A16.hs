{-# LANGUAGE BangPatterns #-}

module A16 (a16_input,a16_ans1,a16_ans2,test) where

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
step is = go 0 is
  where
    go _ [] = []
    go n (i:is') = let s = fst $ foldl' (\(acc,d) x -> case factor n d of
                                                             0 -> (acc,d+1)
                                                             f -> (acc+f*x,d+1)) (0,n) (i:is')
                       digit = (abs s) `rem` 10
                   in digit : go (n+1) is'

step' :: Int -> Int -> [Int] -> [Int]
step' len p is = go 0
  where
    go n
      | n == len = []
      | otherwise =
            let p' = lcm p (4*(n+1))
                r  = len `rem` p'
                xs = take r $ zip (concat $ repeat is) [0..]
                s' = foldl' (\acc (x,d) -> case factor n d of
                                                 0 -> acc
                                                 f -> acc + f*x) 0 xs
                s = (abs s') `rem` 10
            in s : go (n+1)
  

steps :: Int -> [Int] -> [Int]
steps 0 xs = xs
steps n xs = steps (n-1) xs'
  where
    xs' = step xs

stepsIO :: Int -> [Int] -> IO [Int]
stepsIO 0 xs = return xs
stepsIO n xs = do
                print n
                stepsIO (n-1) xs'
  where
    xs' = step xs

parseList :: String -> [Int]
parseList = fmap (read . pure) . head . lines

digit :: Int -> Int -> [Int] -> Int
digit 0 d xs = xs !! d
digit n d xs = foldl' (\acc x -> abs (acc + x) `rem` 10) 0 $ zipWith (\x d' -> digit (n-1) (d+d') xs) (drop d xs) [0..]

a16_input :: IO [Int]
a16_input = fmap parseList $ readFile fileName

a16_ans1 :: [Int] -> Int
a16_ans1 is = sum $ zipWith (\n x -> x * 10^n) [0..] (reverse first)
  where
    ans = steps 100 is
    first = take 8 ans

test :: [Int]
test = fmap (read . pure) "03036732577212944063491565474664"

a16_ans2 :: [Int] -> Int
a16_ans2 is = read $ concat $ fmap show $ take 8 $ res
  where
    skip = read $ concat $ fmap show $ take 7 is
    xs = concat $ take 10000 $ repeat is
    xs' = reverse $ drop skip xs

    myStep (y:ys) = foldl' (\(a:as) y' -> ((a+y') `rem` 10):a:as) [y] ys

    go 0 ys = ys
    go n ys = let ys' = reverse $ myStep ys
              in ys' `deepseq` go (n-1) ys'

    res = reverse $ go 100 xs'
