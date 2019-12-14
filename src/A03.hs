module A03 (a03_input, a03_ans1, a03_ans2) where

data Dir = U Int | R Int | D Int | L Int deriving Show

parseDir :: String -> Dir
parseDir (x:xs) = case x of
                    'U' -> U (read xs)
                    'R' -> R (read xs)
                    'D' -> D (read xs)
                    'L' -> L (read xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a [] = []
splitOn a (x:xs)
    | x == a = splitOn a xs
    | otherwise = let (word, xs') = (takeWhile (/= a) (x:xs), dropWhile (== a) $ dropWhile (/= a) (x:xs))
                  in word : splitOn a xs'

data Line = H Int Int Int | V Int Int Int deriving (Show, Eq, Ord)
type Pos = (Int, Int)

len :: Line -> Int
len (H _ a b) = abs (a - b)
len (V _ a b) = abs (a - b)

line :: Pos -> Dir -> (Pos, Line)
line (x,y) (U u) = ((x, y+u), V x y (y+u))
line (x,y) (R r) = ((x+r, y), H y x (x+r))
line (x,y) (D d) = ((x, y-d), V x y (y-d))
line (x,y) (L l) = ((x-l, y), H y x (x-l))

fromTurns :: Pos -> [Dir] -> (Pos, [Line])
fromTurns pos = foldl (\(p, ls) d -> let (p', l') = line p d
                                     in (p', l':ls))
                      (pos, [])

fromTurnsWithL :: Pos -> [Dir] -> (Pos, Int, [(Int, Line)])
fromTurnsWithL pos = foldl (\(p, s, ls) d -> let (p', l') = line p d
                                                 s' = s + len l'
                                             in (p', s', (s,l'):ls))
                           (pos, 0, [])

intersection :: Line -> Line -> Maybe Pos
intersection (H _ _ _) (H _ _ _) = Nothing
intersection (V _ _ _) (V _ _ _) = Nothing
intersection (V vx vy1 vy2) (H hy hx1 hx2) = intersection (H hy hx1 hx2) (V vx vy1 vy2)
intersection (H hy hx1 hx2) (V vx vy1 vy2)
    | bx && by = Just (vx, hy)
    | otherwise = Nothing
  where
    (hxMin, hxMax) = (min hx1 hx2, max hx1 hx2)
    (vyMin, vyMax) = (min vy1 vy2, max vy1 vy2)
    bx = vx >= hxMin && vx <= hxMax
    by = hy >= vyMin && hy <= vyMax

intersections :: [Line] -> [Line] -> [Pos]
intersections ls ls' = fmap (\(Just i) -> i) is'
  where
    is = intersection <$> ls <*> ls'
    is' = filter (\i -> case i of
                          Nothing -> False
                          _       -> True) is

dist :: Pos -> Pos -> Int
dist (x0, y0) (x,y) = abs (x - x0) + abs (y - y0)

fileName :: String
fileName = "data/a03/input.txt"

a03_input :: IO [[Dir]]
a03_input = fmap (fmap (fmap parseDir . splitOn ',') . lines) $ readFile fileName

a03_ans1 :: [[Dir]] -> Int
a03_ans1 [ds1, ds2] = minimum $ fmap (dist p0) $ intersections ls1 ls2
  where
    p0 = (0, 0)
    [ls1, ls2] = fmap (snd . fromTurns p0) [ds1, ds2]

a03_ans2 :: [[Dir]] -> Int
a03_ans2 [ds1, ds2] = s'
  where
    p0 = (0, 0)
    [ls1, ls2] = fmap ((\(_, _, x) -> x) . fromTurnsWithL p0) [ds1, ds2]
    steps = (\(s1, l1) (s2, l2) -> case intersection l1 l2 of
                                     Nothing -> Nothing
                                     Just _  -> Just $ (s1 + s2, (l1, l2)))
            <$> ls1 <*> ls2
    steps' = filter (\x -> case x of
                             Nothing -> False
                             _       -> True) steps
    (s, (l1, l2)) = minimum $ fmap (\(Just x) -> x) steps'
    s' = case l1 of
           H hy hx1 hx2 ->
             case l2 of
               V vx vy1 vy2 ->
                 s + abs (vx - hx1) + abs (hy - vy1)
           V vx vy1 vy2 ->
             case l2 of
               H hy hx1 hx2 ->
                 s + abs (vx - hx1) + abs (hy - vy1)
