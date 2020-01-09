module A18 (a18_input,a18_ans1,a18_ans2) where

import Prelude hiding (lookup,truncate)

import qualified Data.PSQueue as PSQ

import Data.Maybe (catMaybes)
import Data.List (foldl')
import qualified Data.Bits as B

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad (guard)

import Data.Char (toLower,ord)

type Map = String
type Bounds = ((Int,Int), (Int,Int))
type Pos = (Int,Int)
type Walls = S.Set Pos
type Keys = M.Map Pos Char
type Doors = M.Map Pos Char
type Explored = S.Set Pos

data InfInt = F Int | Inf deriving Eq

instance Show InfInt where
  show Inf = "Inf"
  show (F i) = show i

instance Ord InfInt where
  F i `compare` F j = i `compare` j
  F _ `compare` Inf = LT
  Inf `compare` F _ = GT
  Inf `compare` Inf = EQ

instance Num InfInt where
  F i + F j = F $ i+j
  _   + _   = Inf

  _ - _ = error ""
  _ * _ = error ""
  negate _ = error ""
  abs _ = error ""
  signum _ = error ""
  fromInteger i = F $ fromInteger i

type Vertex = (Char,Int)

data Tree a b = Node a [Tree a b] | Leaf b deriving Show

alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyz"

neighs :: M.Map (Char,Char) (Int,Int) -> Vertex -> [(Vertex,Int)]
neighs ps (k,ks) = catMaybes $
                   fmap (\k' -> do
                                  (d,ds) <- M.lookup (sortPair (k,k')) ps
                                  let ds' = ds B..&. B.complement ks
                                  guard $ ds' == B.zeroBits
                                  return ((k',setBit k' ks), d)) alphabet

neighs' :: [M.Map (Char,Char) (Int,Int)] -> ([Char],Int) -> [(([Char],Int),Int)]
neighs' pss (ks,collected) =
    concat $ 
    fmap (\i -> let ps = pss !! i
                    k  = ks !! i
                in catMaybes $
                   fmap (\k' -> do
                                  (d,ds) <- M.lookup (sortPair (k,k')) ps
                                  let ds' = ds B..&. B.complement collected
                                  guard $ ds' == B.zeroBits
                                  return ((insertAt i k' ks,setBit k' collected), d)) alphabet)
         r
  where
    l = length ks
    r = [0..l-1]

    insertAt _ x [] = error ""
    insertAt 0 x (y:ys) = x:ys
    insertAt i x (y:ys) = y : insertAt (i-1) x ys

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- xs'] ++ xs'
  where
    xs' = powerset xs

setBit :: Char -> Int -> Int
setBit c b = B.bit i B..|. b
  where
    i = ord c - 97

toBits :: [Char] -> Int
toBits = foldl' (flip setBit) B.zeroBits

solve :: [Char] -> M.Map (Char,Char) (Int,Int) -> InfInt
solve ks ps = go queue dists exps
  where
    allKeys = toBits ks
    source = ('@', B.zeroBits)
    dists = M.singleton source 0
    queue = PSQ.singleton source 0
    exps = S.singleton ('@', B.zeroBits)

    go _queue _dists _exps =
      case PSQ.minView _queue of
        Nothing -> error ""
        Just (_vertex@(_v,_ks) PSQ.:-> d0, _queue') ->
          let neighbours = filter (\v -> fst v `S.notMember` _exps) $ neighs ps _vertex
          in 
             if _ks == allKeys
             then d0
             else let (_queue'',_dists') = foldl' (\(_qAcc,_dAcc) (_v'@(n,_),d01) ->
                                                       let alt = d0 + F d01
                                                           _d1 = M.lookup _v' _dists
                                                           d1 = case _d1 of
                                                                  Nothing -> Inf
                                                                  Just i  -> i
                                                       in if alt < d1
                                                          then (PSQ.alter (const $ Just alt) _v' _qAcc,
                                                                M.insert _v' alt _dAcc)
                                                          else (PSQ.alter (const $ Just d1) _v' _qAcc,_dAcc)) (_queue',_dists) neighbours
                      _exps' = S.insert _vertex _exps
                  in go _queue'' _dists' _exps'

solve' :: [Char] -> [M.Map (Char,Char) (Int,Int)] -> InfInt
solve' ks pss = go queue dists exps
  where
    allKeys = toBits ks
    source = (fmap (const '@') pss, B.zeroBits)
    dists = M.singleton source 0
    queue = PSQ.singleton source 0
    exps = S.singleton source

    go _queue _dists _exps =
      case PSQ.minView _queue of
        Nothing -> error ""
        Just (_vertex@(_v,_ks) PSQ.:-> d0, _queue') ->
          let neighbours = filter (\v -> fst v `S.notMember` _exps) $ neighs' pss _vertex
          in 
             if _ks == allKeys
             then d0
             else let (_queue'',_dists') = foldl' (\(_qAcc,_dAcc) (_v'@(n,_),d01) ->
                                                       let alt = d0 + F d01
                                                           _d1 = M.lookup _v' _dists
                                                           d1 = case _d1 of
                                                                  Nothing -> Inf
                                                                  Just i  -> i
                                                       in if alt < d1
                                                          then (PSQ.alter (const $ Just alt) _v' _qAcc,
                                                                M.insert _v' alt _dAcc)
                                                          else (PSQ.alter (const $ Just d1) _v' _qAcc,_dAcc)) (_queue',_dists) neighbours
                      _exps' = S.insert _vertex _exps
                  in go _queue'' _dists' _exps'

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x,y)
  | x < y = (x,y)
  | x == y = (x,y)
  | x > y = (y,x)

paths :: Pos -> Bounds -> Walls -> Keys -> Doors -> M.Map (Char,Char) (Int,Int)
paths pos bounds walls keys doors = M.fromList (firsts ++ ps)
  where
    numkeys = length keys
    pairs = [ (k1,k2) |
                x <- [0..numkeys-1], y <- [x+1..numkeys-1],
                let k1 = M.elemAt x keys, let k2 = M.elemAt y keys]
    firsts = filter ((== B.zeroBits) . snd . snd) $ fmap (\kv -> let (d,ds) = find pos bounds walls keys doors kv
                                                                 in (('@', kv),(d,toBits ds))) $ M.elems keys
    ps = fmap (\((kp1,kv1), (_,kv2)) -> let (d,ds) = find kp1 bounds walls keys doors kv2
                                        in (sortPair (kv1, kv2),(d,toBits ds))) pairs

paths' :: Pos -> Bounds -> Walls -> Keys -> Doors -> M.Map (Char,Char) (Int,Int)
paths' pos bounds walls keys doors = M.fromList (firsts ++ ps)
  where
    numkeys = length keys
    pairs = [ (k1,k2) |
                x <- [0..numkeys-1], y <- [x+1..numkeys-1],
                let k1 = M.elemAt x keys, let k2 = M.elemAt y keys]
    firsts = fmap (\kv -> let (d,ds) = find pos bounds walls keys doors kv
                          in (('@', kv),(d,toBits ds))) $ M.elems keys
    ps = fmap (\((kp1,kv1), (_,kv2)) -> let (d,ds) = find kp1 bounds walls keys doors kv2
                                        in (sortPair (kv1, kv2),(d,toBits ds))) pairs

find :: Pos -> Bounds -> Walls -> Keys -> Doors -> Char -> (Int, [Char])
find pos bounds walls keys doors key
    | null bs' = error ""
    | otherwise = case snd (head bs') == snd (last bs') of
                    False -> error ""
                    True  -> head bs'
  where
    Just t = truncate $ explore pos bounds walls keys doors S.empty key 0
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
    Node (M.lookup pos doors) $ fmap explore' $ next pos
  where
    inBounds (x,y) = x >= x0 && x <= x1 && y >= y0 && y <= y1
    isWall pos' = pos' `S.member` walls
    isDoor pos' = case M.lookup pos' doors of
                    Nothing -> False
                    _       -> True
    isExplored pos' = pos' `S.member` explored

    next (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

    explore' pos' =
      case M.lookup pos' doors of
        Just d -> explore pos' bounds walls keys doors (S.insert pos explored) key (depth+1)
        Nothing -> case M.lookup pos' keys of
                     Nothing -> case isWall pos' || not (inBounds pos') of
                                  True -> Node Nothing []
                                  False -> case isExplored pos' of
                                             True -> Node Nothing []
                                             False -> explore pos' bounds walls keys doors (S.insert pos explored) key (depth+1)
                     Just k -> if k == key
                               then Leaf (depth+1)
                               else case isWall pos' || not (inBounds pos') of
                                      True -> Node Nothing []
                                      False -> case isExplored pos' of
                                                 True -> Node Nothing []
                                                 False -> explore pos' bounds walls keys doors (S.insert pos explored) key (depth+1)

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
                      
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort xl ++ [x] ++ sort xr
  where
    xl = filter (<=x) xs
    xr = filter (>x) xs

elem' :: Ord a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y < x = False
  | y == x = True
  | y > x = y `elem'` xs

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
    walls = S.fromList $ filter (\(x,y) -> (ls !! y) !! x == '#') grid
    keys = M.fromList $
           filter ((`elem'` "abcdefghijklmnopqrstuvwxyz") . snd) $
           fmap (\(x,y) -> ((x,y), ls !! y !! x)) grid
    doors = M.fromList $
            fmap (fmap toLower) $
            filter ((`elem'` "ABCDEFGHIJKLMNOPQRSTUVWXYZ") . snd) $
            fmap (\(x,y) -> ((x,y), ls !! y !! x)) grid

a18_input :: IO Map
a18_input = readFile fileName

a18_ans1 :: Map -> Int
a18_ans1 i = (\x -> case x of
                      Inf -> -1
                      F i -> i) $
             solve (M.elems ks) ps
  where
    (p,bs,ws,ks,ds) = getMaps i
    ps = paths p bs ws ks ds

a18_ans2 :: Map -> Int
a18_ans2 i = (\x -> case x of
                      Inf -> -1
                      F i -> i) $
             solve' allKeys [ps1,ps2,ps3,ps4]
  where
    ls = lines i
    l1:l2:l3:ls' = drop 39 ls
    l1' = take 39 l1 ++ "@#@" ++ drop 42 l1
    l2' = take 39 l2 ++ "###" ++ drop 42 l2
    l3' = take 39 l3 ++ "@#@" ++ drop 42 l3
    i' = take 39 ls ++ [l1'] ++ [l2'] ++ [l3'] ++ ls'

    i1 = unlines $ take 41 $ fmap (take 41) i'
    i2 = unlines $ take 41 $ fmap (drop 40) i'
    i3 = unlines $ drop 40 $ fmap (take 41) i'
    i4 = unlines $ drop 40 $ fmap (drop 40) i'

    (p1,bs1,ws1,ks1,ds1) = getMaps i1
    (p2,bs2,ws2,ks2,ds2) = getMaps i2
    (p3,bs3,ws3,ks3,ds3) = getMaps i3
    (p4,bs4,ws4,ks4,ds4) = getMaps i4

    ps1 = paths' p1 bs1 ws1 ks1 ds1
    ps2 = paths' p2 bs2 ws2 ks2 ds2
    ps3 = paths' p3 bs3 ws3 ks3 ds3
    ps4 = paths' p4 bs4 ws4 ks4 ds4

    allKeys = S.toList $ S.fromList $ concat $ [M.elems ks1, M.elems ks2, M.elems ks3, M.elems ks4]
