{-# LANGUAGE BangPatterns #-}

module A22 (a22_input,a22_ans1,a22_ans2) where

data Act = DealNew | Cut Int | DealIncr Int deriving Show

parseAct :: String -> Act
parseAct s
  | s == "deal into new stack" = DealNew
  | take 4 s == "cut " = let i = read (drop 4 s)
                         in Cut i
  | take 20 s == "deal with increment " = let i = read (drop 20 s)
                                          in DealIncr i
  | otherwise = error s

cAct :: Integral a => a -> Act -> ((a,a) -> (a,a))
cAct m DealNew      = \(a,b) -> ((-a) `mod` m, (-(b+1)) `mod` m)
cAct m (Cut k)      = \(a,b) -> (a, (b-(fromIntegral k)) `mod` m)
cAct m (DealIncr k) = \(a,b) -> ((a*(fromIntegral k)) `mod` m, (b*(fromIntegral k)) `mod` m)

coeffs :: Integral a => a -> [Act] -> (a,a)
coeffs m acts = go funs (1,0)
  where
    funs = fmap (cAct m) acts

    go [] x = x
    go (f:fs) p = let !p' = f p
                  in go fs p'

inverseCoeffs :: Integral a => a -> (a,a) -> (a,a)
inverseCoeffs m (a,b) = (a', ((-b)*a') `mod` m)
  where
    a' = powMod m a (m-2)

coeffsN :: Integral a => a -> a -> (a,a) -> (a,a)
coeffsN m n (a,b) = (a', b')
  where
    a' = aToN
    b' = (((b * (1-aToN)) `mod` m) * inv) `mod` m
    aToN = powMod m a n
    inv = powMod m ((1-a) `mod` m) (m-2)

powMod :: Integral a => a -> a -> a -> a
powMod m 0 k = 0
powMod m _ 0 = 1
powMod m n 1 = n `mod` m
powMod m n k
    | r == 0    = powMod m n2 k2
    | otherwise = (n * powMod m n2 k2) `mod` m
  where
    (k2,r) = k `quotRem` 2
    n2 = (n*n) `rem` m

filename :: String
filename = "data/a22/input.txt"

a22_input :: IO [Act]
a22_input = fmap (fmap parseAct . lines) $ readFile filename

a22_ans1 :: [Act] -> Int
a22_ans1 acts = (a*card + b) `mod` prime
  where
    prime = 10007
    (a,b) = coeffs prime acts
    card = 2019

a22_ans2 :: [Act] -> Integer
a22_ans2 acts = (a'' * 2020 + b'') `mod` prime
  where
    prime = 119315717514047
    n     = 101741582076661
    card  = 2020
    (a,b) = coeffs prime acts
    (a',b') = coeffsN prime n (a,b)
    (a'',b'') = inverseCoeffs prime (a',b')
