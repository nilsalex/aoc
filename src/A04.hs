module A04 (a04_ans1, a04_ans2) where

rmin :: Int
rmin = 372037

rmax :: Int
rmax = 905107

r :: [Int]
r = [372037..905107]

predicate :: Ord a => [a] -> Bool
predicate xs = pred' False xs
  where
    pred' b [] = b
    pred' b (x:[]) = b
    pred' b (x:y:xs) =
      case compare x y of
        LT -> pred' b (y:xs)
        EQ -> pred' True (y:xs)
        GT -> False

predicate' :: Ord a => [a] -> Bool
predicate' xs = any (==2) $ reps xs
  where
    reps [] = []
    reps (x:xs) = (1 + length (takeWhile (==x) xs)) : reps (dropWhile (==x) xs)

a04_ans1 :: Int
a04_ans1 = length $ filter (predicate . show) r

a04_ans2 :: Int
a04_ans2 = length $ filter (predicate' . show) $ filter (predicate . show) r
