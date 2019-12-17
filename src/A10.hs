module A10 (a10_input,a10_ans1,a10_ans2) where

import Data.Ratio

newtype State s a = State { runState :: s -> (a, s) }

getS :: State s s
getS = State $ \s -> (s, s)

putS :: s -> State s ()
putS s = State $ \_ -> ((), s)

modifyS :: (s -> s) -> State s ()
modifyS f = State $ \s -> ((), f s)

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a', s') = g s
                                   in (f a', s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State fs <*> State g =
    State $ \s -> let (fa, s') = fs s
                      (a, s'') = g s'
                  in (fa a, s'')

instance Monad (State s) where
  return = pure
  State f >>= g = State $ \s -> let (a', s') = f s
                                in runState (g a') s'

data Val = Space | Asteroid deriving Show
data Map = Map Int Int [Val] deriving Show
data Arg a = Q1 a | Q2 a | Q3 a | Q4 a deriving (Show, Eq, Ord)

polar :: (Int, Int) -> (Arg Rational, Int)
polar (x,y) = (arg x y, gcd x y)

arg :: Int -> Int -> Arg Rational
arg x' y'
    | y > 0 && x >= 0 = Q1 $ x % y
    | y <= 0 && x > 0 = Q2 $ abs y % x
    | y < 0 && x <= 0 = Q3 $ abs x % abs y
    | y >= 0 && x < 0 = Q4 $ y % abs x
    | otherwise = error "cannot compute arg of (0,0)"
  where
   y = fromIntegral $ negate y'
   x = fromIntegral x'

(.!) :: Map -> (Int, Int) -> Val
Map rdim _ vs .! (x,y) = vs !! (y * rdim + x)

parseMap :: String -> Map
parseMap s = Map rd cd $
             fmap (\c -> case c of
                           '.' -> Space
                           '#' -> Asteroid) $
             concat ls
  where
    ls = lines s
    rd = length $ head ls
    cd = length ls

asteroids :: Map -> [(Int, Int)]
asteroids m@(Map rdim cdim _) =
  fmap fst $
  filter (\(_, v) -> case v of
                       Asteroid -> True
                       Space -> False) $
  (\x y -> ((x, y), m .! (x,y))) <$> [0..rdim-1] <*> [0..cdim-1]

voids :: Map -> [(Int, Int)]
voids m@(Map rdim cdim _) =
  fmap fst $
  filter (\(_, v) -> case v of
                       Asteroid -> False
                       Space -> True) $
  (\x y -> ((x, y), m .! (x,y))) <$> [0..rdim-1] <*> [0..cdim-1]

elem' :: Ord a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
  | x < y  = False
  | x == y = True
  | x > y  = elem' x ys

remove' :: Ord a => a -> [a] -> [a]
remove' x [] = []
remove' x (y:ys)
  | x < y = y:ys
  | x == y = ys
  | x > y = y : remove' x ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
  where
    ls = filter (<=x) xs
    rs = filter (>x) xs

group' :: [((Arg Rational, Int), (Int, Int))] -> [(Arg Rational, [(Int, (Int, Int))])]
group' [] = []
group' (x:xs) = (phi, g') : group' nonG
  where
    ((phi, r), c) = x
    g = takeWhile (\((phi', _), _) -> phi == phi') (x:xs)
    nonG = dropWhile (\((phi', _), _) -> phi == phi') (x:xs)
    g' = fmap (\((_, r), c) -> (r, c)) g

inSight :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
inSight as (vx, vy) (ax, ay)
    | (lx, ly) == (0,0) = False
    | g == 1 = True
    | otherwise = all (not . (`elem` as)) cands
  where
    (lx, ly) = (ax-vx, ay-vy)
    g = gcd lx ly
    lx' = lx `div` g
    ly' = ly `div` g
    cands = fmap (\i -> (vx + i*lx', vy + i*ly')) [1..g-1]

fileName :: String
fileName = "data/a10/input.txt"

loadAsts :: String -> IO [(Int,Int)]
loadAsts = fmap (asteroids . parseMap) . readFile

destroy' :: Ord a => a -> [(a, [b])] -> Maybe ([(a, [b])], b)
destroy' _ [] = Nothing
destroy' phi ((phi',bs):xs)
    | phi < phi'  = Nothing
    | phi == phi' = case bs of
                      b:[] -> Just (xs, b)
                      b:bs' -> Just ((phi,bs'):xs, b)
    | phi > phi'  = do
                      (xs',b) <- destroy' phi xs
                      return $ ((phi',bs):xs',b)

next :: Ord a => a -> [(a, b)] -> Maybe a
next a [] = Nothing
next a ((x,_):xs)
    | a < x     = Just x
    | otherwise = next a xs

nextPhi :: Ord a => State (a, [(a, b)]) ()
nextPhi = do
           (phi, xs) <- getS
           case next phi xs of
             Nothing -> case xs of
                          [] -> return ()
                          (phi', _):_ -> putS (phi', xs)
             Just phi' -> putS (phi', xs)

destroy :: Ord a => State (a, [(a, [(b, c)])]) (Maybe c)
destroy = do
           (phi, xs) <- getS
           case destroy' phi xs of
             Nothing -> do return Nothing
             Just (xs',(_,b))  -> do
                                   modifyS (fmap (const xs'))
                                   nextPhi
                                   return $ Just b

destroyN :: Ord a => Int -> State (a, [(a, [(b, c)])]) (Maybe c)
destroyN 0 = return Nothing
destroyN n = do
              destroyN (n-1)
              destroy

state0 :: [(Int, Int)] -> (Arg Rational, [(Arg Rational, [(Int, (Int, Int))])])
state0 asts = (Q1 $ 0 % 1, astsByArg)
  where
    ast@(ax,ay) = snd $ maximum $ fmap (\a -> (length (filter (inSight asts a) asts), a)) asts
    asts' = remove' ast asts
    astsRel = fmap (\(x,y) -> ((x - ax, y - ay), (x,y))) asts'
    astsPolar = sort $ fmap (\(r, c) -> (polar r, c)) astsRel
    astsByArg = group' astsPolar

a10_input :: IO [(Int,Int)]
a10_input = loadAsts fileName

a10_ans1 :: [(Int,Int)] -> Int
a10_ans1 asts = maximum $ fmap (\a -> length (filter (inSight asts a) asts)) asts

a10_ans2 :: [(Int,Int)] -> Int
a10_ans2 asts = 100 * x + y
  where
    s0 = state0 asts
    Just (x,y) = fst $ runState (destroyN 200) $ s0
