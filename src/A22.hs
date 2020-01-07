{-# LANGUAGE BangPatterns #-}

module A22 (a22_input,a22_ans1,a22_ans2) where

data Act = DealNew | Cut Int | DealIncr Int deriving Show

parseAct :: String -> Act
parseAct s
  | s == "deal into new stack" = DealNew
  | take 4 s == "cut " = let i = read (drop 4 s) :: Int
                         in Cut i
  | take 20 s == "deal with increment " = let i = read (drop 20 s) :: Int
                                         in DealIncr i
  | otherwise = error s

mAct :: Int -> Act -> (Int -> Int)
mAct m DealNew      = \i -> (-(i+1)) `mod` m
mAct m (Cut k)      = \i -> (i-k) `mod` m
mAct m (DealIncr k) = \i -> (i*k) `mod` m

filename :: String
filename = "data/a22/input.txt"

a22_input :: IO [Act]
a22_input = fmap (fmap parseAct . lines) $ readFile filename

a22_ans1 :: [Act] -> Int
a22_ans1 acts = go funs card
  where
    prime = 10007
    funs = fmap (mAct prime) acts
    card = 2019

    go [] c = c
    go (a:as) c = let !c' = a c in go as c'

a22_ans2 :: [Act] -> Int
a22_ans2 acts = (-1)
