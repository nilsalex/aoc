{-# LANGUAGE BangPatterns #-}

module A22 (a22_input,a22_ans1,a22_ans2) where

import qualified Data.Sequence as S
import Data.List (sort,foldl')

import Debug.Trace (trace)

cut :: Int -> S.Seq a -> S.Seq a
cut n xs = case n `compare` 0 of
             LT -> S.drop (n+l) xs S.>< S.take (n+l) xs
             EQ -> xs
             GT -> S.drop n xs S.>< S.take n xs
  where
    l = S.length xs

dealWithIncrement :: Int -> S.Seq a -> S.Seq a
dealWithIncrement i xs = S.fromList $ foldr (\i acc -> S.index xs i : acc) [] perm
  where
    n = S.length xs
    perm = fmap snd $ sort $ zip (take n $ go 0) [0..]
    go x = let x' = (x + i) `mod` n
           in x : go x'

parseAct :: String -> (S.Seq a -> S.Seq a)
parseAct s
  | s == "deal into new stack" = S.reverse
  | take 4 s == "cut " = let i = read (drop 4 s) :: Int
                         in cut i
  | take 20 s == "deal with increment " = let i = read (drop 20 s) :: Int
                                         in dealWithIncrement i
  | otherwise = error s

filename :: String
filename = "data/a22/input.txt"

a22_input :: IO [S.Seq a -> S.Seq a]
a22_input = fmap (fmap parseAct . lines) $ readFile filename

a22_ans1 :: [S.Seq Int -> S.Seq Int] -> Int
a22_ans1 acts = (\(Just x) -> x) $ S.elemIndexL 2019 deck'
  where
    deck = S.fromList [0..10006]

    deck' = go acts deck

    go [] d = d
    go (a:as) d = let !a' = a d in go as a'

a22_ans2 :: [S.Seq Int -> S.Seq Int] -> Int
a22_ans2 acts = (-1)
