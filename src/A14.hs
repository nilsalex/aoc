module A14 (a14_input,a14_ans1,a14_ans2) where

data Reactant = Reactant { substance :: String, quantity :: Int } deriving (Show, Ord, Eq)
data Reaction = Reaction { right :: Reactant, left :: [Reactant] } deriving (Show, Ord, Eq)

substitute :: ([Reaction], [Reactant]) -> ([Reaction], [Reactant])
substitute (reactions, reactants) = (reactions', foldr (mergeWith 1) canNotSub substituted)
  where
    canSub = filter (\r -> count r reactions == 0) reactants
    canNotSub = filter (\r -> count r reactions /= 0) reactants

    substituted = fmap (\r@(Reactant n _) ->
                             let replacement = head $ filter (\(Reaction (Reactant n' _) _) -> n == n') reactions
                                 factor = quantity r `div'` quantity (right replacement)
                             in multiply factor (left replacement)) canSub

    reactions' = foldr remove' reactions canSub

    count r rs = length $ filter (isInLeft r) rs

multiply :: Int -> [Reactant] -> [Reactant]
multiply f = fmap (\(Reactant n q) -> Reactant n (f*q))

mergeWith :: Int -> [Reactant] -> [Reactant] -> [Reactant]
mergeWith _ xs [] = xs
mergeWith _ [] xs = xs
mergeWith f (x@(Reactant n1 q1):xs) (y@(Reactant n2 q2):ys)
  | n1 < n2 = x : mergeWith f xs (y:ys)
  | n1 == n2 = (Reactant n1 (q1 + f * q2)) : mergeWith f xs ys
  | n1 > n2 = y : mergeWith f (x:xs) ys

div' :: Int -> Int -> Int
div' a b
  | a `mod` b == 0 = a `div` b
  | otherwise = (a `div` b) + 1

parseReaction :: String -> Reaction
parseReaction l = Reaction (last l'') (sort $ init l'')
  where
    l' = filter (\c -> not $ c `elem` ",=>") l
    l'' = fmap (\[a,b] -> Reactant b (read a)) $ chunksOf 2 $ splitAt' ' ' l'

isInLeft :: Reactant -> Reaction -> Bool
isInLeft _ (Reaction _ []) = False
isInLeft r@(Reactant n _) (Reaction p (r'@(Reactant n' _):rs))
  | n < n' = False
  | n == n' = True
  | n > n' = isInLeft r (Reaction p rs)

remove' :: Reactant -> [Reaction] -> [Reaction]
remove' _ [] = []
remove' r@(Reactant n q) (x@(Reaction (Reactant n' q') _):xs)
  | n < n' = x:xs
  | n == n' = xs
  | n > n' = x : remove' r xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
  where
    ls = filter (<=x) xs
    rs = filter (>x) xs

splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' _ [] = []
splitAt' x (y:ys)
  | x == y = splitAt' x ys
  | otherwise = takeWhile (/=x) (y:ys) : splitAt' x (dropWhile (/=x) ys)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] =  []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

getFuel :: [Reaction] -> [Reactant]
getFuel = left . head . filter (\(Reaction (Reactant n _) _) -> n == "FUEL")

getRem :: [Reaction] -> [Reaction]
getRem = remove' (Reactant "FUEL" 1)

fileName :: String
fileName = "data/a14/input.txt"

a14_input :: IO [Reaction]
a14_input = do
             content <- readFile fileName
             return $ sort $ fmap (parseReaction) $ lines content

a14_ans1 :: [Reaction] -> Int
a14_ans1 rs = quantity (head $ snd $ solution)
  where
    s0 = (getRem rs, getFuel rs)
    results = iterate substitute s0
    solution = head $ dropWhile (not . null . fst) results

a14_ans2 :: [Reaction] -> Int
a14_ans2 _ = 0
