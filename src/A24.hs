module A24 (a24_input,a24_ans1,a24_ans2) where

import qualified Data.Set as S
import Data.Foldable (foldl')

type Pos = (Int, (Int,Int))

step' :: S.Set Pos -> S.Set Pos
step' bugs =
    foldl' (\acc pos -> let ns = length $ filter (\p -> p `S.member` bugs) $ neighs pos
                        in if pos `S.member` bugs
                           then if ns == 1
                                then pos `S.insert` acc
                                else acc
                           else if ns == 1 || ns == 2
                                then pos `S.insert` acc
                                else acc) S.empty toInspect
  where
    bugList = S.toList bugs
    toInspect = S.fromList $ concat $ fmap neighs bugList

getMap :: [String] -> S.Set Pos
getMap ls = S.fromList bugs
  where
    bugs = fmap fst $
           filter ((/= (2,2)) . snd . fst) $
           filter ((== '#') . snd) $
           (\y x -> ((0,(x,y)),(ls !! y) !! x)) <$> [0..4] <*> [0..4]

neighs :: Pos -> [Pos]
neighs (depth, (x,y)) = up ++ down ++ left ++ right
  where
    up = case (x,y) of
           (_,0) -> [(depth-1,(2,1))]
           (2,3) -> [(depth+1,(i,4)) | i <- [0..4]]
           _     -> [(depth,(x,y-1))]
    down = case (x,y) of
             (2,1) -> [(depth+1,(i,0)) | i <- [0..4]]
             (_,4) -> [(depth-1,(2,3))]
             _     -> [(depth,(x,y+1))]
    left = case (x,y) of
             (0,_) -> [(depth-1,(1,2))]
             (3,2) -> [(depth+1,(4,i)) | i <- [0..4]]
             _     -> [(depth,(x-1,y))]
    right = case (x,y) of
              (1,2) -> [(depth+1,(0,i)) | i <- [0..4]]
              (4,_) -> [(depth-1,(3,2))]
              _     -> [(depth,(x+1,y))]

rating :: Int -> Int -> [String] -> Int
rating xRange yRange ls = sum $ zipWith (\x i -> x * (2^i)) coeffs [0..]
  where
    coeffs = (\y x -> case (ls !! y) !! x of
                        '.' -> 0
                        '#' -> 1) <$> [0..yRange-1] <*> [0..xRange-1]

step :: Int -> Int -> [String] -> [String]
step xRange yRange ls =
    fmap (\y ->
      fmap (\x ->
             let tile = lookup x y
                 n    = adjs x y
             in if tile == '#'
                then if n == 1
                     then '#'
                     else '.'
                else if n == 1 || n == 2
                     then '#'
                     else '.') [0..xRange-1]) [0..yRange-1]
  where
    adjs x y = length $
               filter (== '#') $ fmap (\(x',y') -> lookup x' y') [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    lookup x y
      | x >= 0 && x < xRange && y >= 0 && y < yRange = (ls !! y) !! x
      | otherwise = '.'

fileName :: String
fileName = "data/a24/input.txt"

a24_input :: IO [String]
a24_input = fmap lines $ readFile fileName

a24_ans1 :: [String] -> Int
a24_ans1 i = go i S.empty
  where
    xRange = length (head i)
    yRange = length i

    go x rs = let x' = step xRange yRange x
                  r' = rating xRange yRange x'
              in if r' `S.member` rs
                 then r'
                 else go x' (r' `S.insert` rs)

a24_ans2 :: [String] -> Int
a24_ans2 i = S.size $ go 200 m
  where
    m = getMap i

    go 0 m = m
    go n m = let m' = step' m
             in go (n-1) m'
