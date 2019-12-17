{-# LANGUAGE BangPatterns #-}

module A12 (a12_ans1,a12_ans2) where

import Data.List (foldl')

type Pos = (Int, Int, Int)

initial :: [(Pos, Pos)]
initial = [((-4, 3, 15), (0,0,0)),
           ((-11, -10, 13), (0,0,0)),
           ((2, 2, 18), (0,0,0)),
           ((7, -1, 0), (0,0,0))]

initial' :: [(Pos, Pos)]
initial' = [((-1,0,2), (0,0,0)),
            ((2,-10,-7), (0,0,0)),
            ((4,-8,8), (0,0,0)),
            ((3,5,-1), (0,0,0))]

initial0 :: [(Pos, Pos)]
initial0 = [((0,0,0), (0,0,0))]

gravity :: (Pos, Pos) -> (Pos, Pos) -> (Pos, Pos)
gravity ((x1, y1, z1), (vx1, vy1, vz1)) ((x2, y2, z2), (vx2, vy2, vz2)) =
    ((x1, y1, z1), (vx1 + fx, vy1 + fy, vz1 + fz))
  where
    f !a !b
      | a < b = 1
      | a == b = 0
      | a > b = -1
    !fx = f x1 x2
    !fy = f y1 y2
    !fz = f z1 z2

velocity :: (Pos, Pos) -> (Pos, Pos)
velocity ((x,y,z), (vx,vy,vz)) = p'
  where
    !x' = (x+vx,y+vy,z+vz)
    !v' = (vx,vy,vz)
    !p' = (x',v')

energy :: (Pos, Pos) -> Int
energy ((x,y,z), (vx,vy,vz)) = res
  where
    !pot = (abs x + abs y + abs z)
    !kin = (abs vx + abs vy + abs vz)
    !res = pot * kin

total :: [(Pos, Pos)] -> Int
total = sum . fmap energy

step :: [(Pos, Pos)] -> [(Pos, Pos)]
step xs = fmap (\x -> let !g = foldl' gravity x xs
                      in  velocity g) xs

simulate :: Int -> [(Pos, Pos)] -> [(Pos, Pos)]
simulate 0 = id
simulate n = simulate (n-1) . step

history :: [(Pos, Pos)] -> [[(Pos, Pos)]]
history i = i : history (step i)

findRep :: [(Pos, Pos)] -> Int
findRep i = go 0 [i]
  where
    go n (x:hs) =
      let x' = step x in
      case x' `elem` hs of
        True -> (n+1)
        False -> go (n+1) (x':x:hs)

a12_ans1 :: Int
a12_ans1 = total $ simulate 1000 $ initial

a12_ans2 :: Int
a12_ans2 = findRep initial
