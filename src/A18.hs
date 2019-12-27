module A18 (a18_input,a18_ans1,a18_ans2) where

import Prelude hiding (lookup,truncate)

import Data.Char (toLower)

import Debug.Trace (trace)

type Map = String
type Bounds = ((Int,Int), (Int,Int))
type Pos = (Int,Int)
type Walls = [(Int,Int)]
type Keys = [(Pos, Char)]
type Doors = [(Pos, Char)]
type Explored = [Pos]

data Tree a b = Node a [Tree a b] | Leaf b deriving Show

showAvail :: ((Char,Char), (Int, [Char])) -> String
showAvail ((k1,k2),_) = [k1] ++ " <---> " ++ [k2]

showBlock :: ((Char,Char), (Int, [Char])) -> String
showBlock ((k1,k2),(_,bs)) = [k1] ++ " <-" ++ bs ++ "-> " ++ [k2]

solve :: [[((Char, Char), (Int, [Char]))]] -> Tree Char Int
solve [fsts, avails, blockeds] =
    Node '@' $ fmap (\((_,k0),(d0,_)) -> let (as',bs') = removeDoor k0 avails blockeds
                                         in go k0 as' bs' d0) fsts
  where
    next k as = filter (\((k1,k2),_) -> k `elem` [k1,k2]) as

    isEdge k ((k1,k2),_) = k `elem` [k1,k2]

    go k as bs d
      | null as = Node k [Leaf d]
      | otherwise = -- trace (show k ++ "\n" ++ unlines (fmap showAvail as) ++ unlines (fmap showBlock bs)) $
                    Node k $
                    fmap (\x@((k1,k2),(d',_)) ->
                              let k' = dest k (k1,k2)
                                  (as',bs') = removeDoor k' as bs
                                  as'' = removeIf (k `isEdge`) as'
                                  bs'' = removeIf (k `isEdge`) bs'
                              in go k' as'' bs'' (d+d')) $
                    next k as
    
    dest k (k1,k2)
      | k == k1 = k2
      | k == k2 = k1
      | otherwise = error ""

removeUnOrd :: Eq a => a -> [a] -> [a]
removeUnOrd _ [] = []
removeUnOrd y (x:xs)
  | x == y = xs
  | otherwise = x : removeUnOrd y xs

removeDoor :: Char -> [((Char,Char), (Int,[Char]))] -> [((Char,Char), (Int,[Char]))] -> ([((Char,Char), (Int,[Char]))], [((Char,Char), (Int,[Char]))])
removeDoor k as bs = foldr (\b@((k1,k2), (d,ds)) (asAcc, bsAcc) ->
                               case removeUnOrd k ds of
                                 [] -> (insert ((k1,k2), (d,[])) asAcc,
                                        bsAcc)
                                 ds' -> (asAcc,
                                         insert ((k1,k2), (d,ds')) $
                                         remove b $ bsAcc))
                           (as,[]) bs

removeIf :: (a -> Bool) -> [a] -> [a]
removeIf _ [] = []
removeIf f (x:xs)
  | f x = removeIf f xs
  | otherwise = x : removeIf f xs

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x,y)
  | x < y = (x,y)
  | x == y = (x,y)
  | x > y = (y,x)

paths :: Pos -> Bounds -> Walls -> Keys -> Doors -> [[((Char, Char), (Int, [Char]))]]
paths pos bounds walls keys doors =
    [sort firsts,
     sort $ filter (null . snd . snd) ps,
     sort $ filter (not . null . snd . snd) ps]
  where
    numkeys = length keys
    pairs = [ (k1,k2) |
                x <- [0..numkeys-1], y <- [x+1..numkeys-1],
                let k1 = keys !! x, let k2 = keys !! y]
    firsts = filter (null . snd . snd) $ fmap (\(_,kv) -> (('@', kv), find pos bounds walls keys doors kv)) keys
    ps = fmap (\((kp1,kv1), (_,kv2)) -> (sortPair (kv1, kv2), find kp1 bounds walls keys doors kv2)) pairs

find :: Pos -> Bounds -> Walls -> Keys -> Doors -> Char -> (Int, [Char])
find pos bounds walls keys doors key
    | null bs' = error ""
    | otherwise = case snd (head bs') == snd (last bs') of
                    False -> error ""
                    True  -> head bs'
  where
    Just t = truncate $ explore pos bounds walls keys doors [] key 0
    bs = branches t
    bs' = sort $ fmap collapse bs

collapse :: [Either (Maybe a) b] -> (b, [a])
collapse xs = (l, fmap (\(Just x) -> x) xs'')
  where
    (Right l) = last xs
    xs' = fmap (\(Left x) -> x) $ init xs
    xs'' = filter (\x -> case x of Nothing -> False; _ -> True) xs'

explore :: Pos -> Bounds -> Walls -> Keys -> Doors -> Explored -> Char -> Int -> Tree (Maybe Char) Int
explore pos bounds@((x0,x1),(y0,y1)) walls keys doors explored key depth =
    Node (lookup pos doors) $ fmap explore' $ next pos
  where
    inBounds (x,y) = x >= x0 && x <= x1 && y >= y0 && y <= y1
    isWall pos' = pos' `elem'` walls
    isDoor pos' = case lookup pos' doors of
                    Nothing -> False
                    _       -> True
    isExplored pos' = pos' `elem'` explored

    next (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

    explore' pos' =
      case lookup pos' doors of
        Just d -> explore pos' bounds walls keys doors (insert pos explored) key (depth+1)
        Nothing -> case lookup pos' keys of
                     Nothing -> case isWall pos' || not (inBounds pos') of
                                  True -> Node Nothing []
                                  False -> case isExplored pos' of
                                             True -> Node Nothing []
                                             False -> explore pos' bounds walls keys doors (insert pos explored) key (depth+1)
                     Just k -> if k == key
                               then Leaf (depth+1)
                               else case isWall pos' || not (inBounds pos') of
                                      True -> Node Nothing []
                                      False -> case isExplored pos' of
                                                 True -> Node Nothing []
                                                 False -> explore pos' bounds walls keys doors (insert pos explored) key (depth+1)

leafs :: Tree a b -> [b]
leafs (Leaf l) = [l]
leafs (Node _ f) = concat $ fmap leafs f

branches :: Tree a b -> [[Either a b]]
branches (Leaf l) = [[Right l]]
branches (Node n fs) = fmap ((Left n):) $ concat $ fmap branches fs

truncate :: Tree a b -> Maybe (Tree a b)
truncate (Leaf l) = Just (Leaf l)
truncate (Node _ []) = Nothing
truncate (Node n f) = case f' of
                        [] -> Nothing
                        f'' -> Just $ Node n f''
  where
    f' = fmap (\(Just x) -> x) $
         filter (\x -> case x of
                         Nothing -> False
                         Just _ -> True) $
         fmap truncate f
                      

lookup :: Ord a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup y ((x,v):xs)
  | y < x = Nothing
  | y == x = Just v
  | y > x = lookup y xs

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
  | a < x = a:x:xs
  | a == x = x:xs
  | a > x = x : insert a xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort xl ++ [x] ++ sort xr
  where
    xl = filter (<=x) xs
    xr = filter (>x) xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted (_:[]) = True
sorted (x:y:xs)
  | x < y = sorted (y:xs)
  | x == y = sorted (y:xs)
  | otherwise = False

elem' :: Ord a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y < x = False
  | y == x = True
  | y > x = y `elem'` xs

remove :: Ord a => a -> [a] -> [a]
remove _ [] = []
remove y (x:xs)
  | y < x = x:xs
  | y == x = xs
  | y > x = x : remove y xs

remove' ::  Char -> [(Pos, Char)] -> [(Pos, Char)]
remove' _ [] = []
remove' key ((posDoor, door):xs)
  | key == door = xs
  | otherwise = (posDoor, door) : remove' key xs

fileName :: String
fileName = "data/a18/input.txt"

getMaps :: String -> (Pos, Bounds, Walls, Keys, Doors)
getMaps input = (pos, bounds, walls, keys, doors)
  where
    ls = lines input
    rows = length ls
    cols = length (head ls)
    grid = (,) <$> [0..cols-1] <*> [0..rows-1]
    pos = head $ filter (\(x,y) -> (ls !! y) !! x == '@') grid
    bounds = ((0,cols-1), (0,rows-1))
    walls = filter (\(x,y) -> (ls !! y) !! x == '#') grid
    keys = filter ((`elem` "abcdefghijklmnopqrstuvwxyz") . snd) $
           fmap (\(x,y) -> ((x,y), ls !! y !! x)) grid
    doors = fmap (fmap toLower) $
            filter ((`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ") . snd) $
            fmap (\(x,y) -> ((x,y), ls !! y !! x)) grid

a18_input :: IO Map
a18_input = readFile fileName

a18_ans1 :: Map -> [String]
a18_ans1 i = fmap show $ branches sol
  where
    (p,bs,ws,ks,ds) = getMaps i
    ps = paths p bs ws ks ds
    sol = solve ps

a18_ans2 :: Map -> Int
a18_ans2 = undefined
