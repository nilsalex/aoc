module A06 (a06_input,a06_ans1,a06_ans2) where

data Tree a = Node a [Tree a] deriving Show

data Edge a = Edge a a deriving Show

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

find :: Eq a => a -> Tree a -> Maybe [a]
find x (Node n ts)
  | x == n = Just [x]
  | otherwise = case dropWhile isNothing (fmap (find x) ts) of
                  [] -> Nothing
                  (Just xs):_ -> Just $ n : xs
                  

parseEdge :: String -> Edge String
parseEdge s = case splitAt' ')' s of
                [n1, n2] -> Edge n1 n2

fromEdges :: Eq a => a -> [Edge a] -> Tree a
fromEdges r es = Node r ts
  where
    es' = filter (\(Edge n1 _) -> n1 == r) es
    ts  = fmap (\(Edge _ n2) -> fromEdges n2 es) es'

depths :: Int -> Tree a -> Tree Int
depths rd (Node _ ts) = Node rd $ fmap (depths (rd+1)) ts

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a []) = f a []
foldTree f (Node a ts) = f a $ fmap (foldTree f) ts

sumTree :: Num a => Tree a -> a
sumTree = foldTree (foldr (+))

dist :: Eq a => [a] -> [a] -> Int
dist [] [] = 0
dist [] xs = length xs
dist xs [] = length xs
dist (x:xs) (y:ys)
  | x == y = dist xs ys
  | otherwise = length (x:xs) + length (y:ys)

splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' d [] = []
splitAt' d xs = case word of
                  [] -> splitAt' d xs
                  _  -> word : splitAt' d xs'
  where
    (word, xs') = (takeWhile (/= d) xs, dropWhile (==d) $ dropWhile (/= d) xs)

fileName :: String
fileName = "data/a06/input.txt"

a06_input :: IO [Edge String]
a06_input = fmap (fmap parseEdge . lines) $ readFile fileName

a06_ans1 :: [Edge String] -> Int
a06_ans1 es = sumTree ds
  where
    tree = fromEdges "COM" es
    ds   = depths 0 tree

a06_ans2 :: [Edge String] -> Int
a06_ans2 es = d - 2
  where
    tree = fromEdges "COM" es
    Just branch_you = find "YOU" tree
    Just branch_san = find "SAN" tree
    d = dist branch_you branch_san
