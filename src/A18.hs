module A18 (a18_input,a18_ans1,a18_ans2) where

import Prelude hiding (lookup,truncate)

import qualified Data.PSQueue as PSQ

import Data.Maybe (catMaybes)
import Data.List (foldl',minimumBy)
import qualified Data.Bits as B

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad (guard)

import Data.Char (toLower,ord)

data InfInt = F Int | Inf deriving Eq

type Pos = (Int,Int)
data Tile = Path | Wall | Key Char | Door Char deriving Show
data Start = Start
data Space = Space
type Maze = M.Map Pos Tile

parseTile :: Char -> Either Tile (Either Start Space)
parseTile '.' = Right (Right Space)
parseTile '#' = Left Wall
parseTile '@' = Right (Left Start)
parseTile c
    | c == c'   = Left (Key c)
    | otherwise = Left (Door c')
  where
    c' = toLower c

parseMaze :: String -> ([Pos],Maze)
parseMaze str =
  where
    ls = lines str
    xDim = maximum $ fmap length ls
    yDim = length ls

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

fileName :: String
fileName = "data/a18/input.txt"

a18_input :: IO String
a18_input = readFile fileName

a18_ans1 :: String -> Int
a18_ans1 i = (-1)

a18_ans2 :: String -> Int
a18_ans2 i = (-1)
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
