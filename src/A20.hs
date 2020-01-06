module A20 (a20_input,a20_ans1,a20_ans2) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (foldl',sort,groupBy,sortBy)
import Debug.Trace (trace)
import Data.Char (isAsciiUpper)
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import qualified Data.PSQueue as PSQ

type Pos = (Int,Int)
type Walls = S.Set Pos

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

type Maze = (S.Set Pos, Pos, Pos, M.Map Pos Pos)
type Grid = (M.Map Pos Char, Int, Int)

data Dir = In | Out deriving Show
type RecMaze = (S.Set Pos, Pos, Pos, M.Map Pos (Pos,Dir))

parseMaze :: String -> Maze
parseMaze i = (paths, entry, exit, portalMap)
  where
    ls = lines i
    rows = length ls
    cols = maximum $ fmap length ls

    grid = foldl' (\ws (x,y) -> case (ls !! y) !! x of
                                  ' ' -> ws
                                  c   -> M.insert (x,y) c ws)
           M.empty $ (,) <$> [0..cols-1] <*> [0..rows-1]
    gridX = maximum $ fmap fst $ M.keys grid
    gridY = maximum $ fmap snd $ M.keys grid

    paths = foldl' (\ws (x,y) -> case (ls !! y) !! x of
                                   '.' -> S.insert (x,y) ws
                                   _   -> ws)
            S.empty $ (,) <$> [0..cols-1] <*> [0..rows-1]

    horLeft = findHorLeft (grid, gridX, gridY)
    horRight = findHorRight (grid, gridX, gridY)
    verUp = findVerUp (grid, gridX, gridY)
    verDown = findVerDown (grid, gridX, gridY)

    portals' = M.assocs $ foldl' M.union M.empty [horLeft,horRight,verUp,verDown]
    entry = fst $ head $ filter ((== ('A','A')) . snd) portals'
    exit  = fst $ head $ filter ((== ('Z','Z')) . snd) portals'
    portals = filter (\(_,name) -> name /= ('A','A') && name /= ('Z','Z')) portals'
    swapped = chunksOf 2 $ sort $ fmap (\(x,y) -> (y,x)) portals

    portalMap = foldl' (\acc [(n1,p1),(n2,p2)] -> if n1 /= n2
                                                  then error ""
                                                  else M.insert p1 p2 $ M.insert p2 p1 acc) M.empty swapped

parseRecMaze :: String -> RecMaze
parseRecMaze i = (paths, entry, exit, portalMap)
  where
    ls = lines i
    rows = length ls
    cols = maximum $ fmap length ls

    grid = foldl' (\ws (x,y) -> case (ls !! y) !! x of
                                  ' ' -> ws
                                  c   -> M.insert (x,y) c ws)
           M.empty $ (,) <$> [0..cols-1] <*> [0..rows-1]
    gridX = maximum $ fmap fst $ M.keys grid
    gridY = maximum $ fmap snd $ M.keys grid

    paths = foldl' (\ws (x,y) -> case (ls !! y) !! x of
                                   '.' -> S.insert (x,y) ws
                                   _   -> ws)
            S.empty $ (,) <$> [0..cols-1] <*> [0..rows-1]

    [hlOut,hlIn] = sortBy (\(((x,_),_):_) (((x',_),_):_) -> x `compare` x') $ groupBy (\((x,_),_) ((x',_),_) -> x == x') $ M.assocs $ findHorLeft (grid, gridX, gridY)
    [hrOut,hrIn] = sortBy (\(((x,_),_):_) (((x',_),_):_) -> x' `compare` x) $ groupBy (\((x,_),_) ((x',_),_) -> x == x') $ M.assocs $ findHorRight (grid, gridX, gridY)
    [vuOut,vuIn] = groupBy (\((_,y),_) ((_,y'),_) -> y == y') $ sortBy (\((_,y),_) ((_,y'),_) -> y `compare` y') $ M.assocs $ findVerUp (grid, gridX, gridY)
    [vdOut,vdIn] = groupBy (\((_,y),_) ((_,y'),_) -> y == y') $ sortBy (\((_,y),_) ((_,y'),_) -> y' `compare` y) $ M.assocs $ findVerDown (grid, gridX, gridY)

    portalsOut' = M.assocs $ foldl' (\acc l -> M.fromList l `M.union` acc) M.empty [hlOut,hrOut,vuOut,vdOut]
    portalsIn = M.assocs $ foldl' (\acc l -> M.fromList l `M.union` acc) M.empty [hlIn,hrIn,vuIn,vdIn]

    entry = fst $ head $ filter ((== ('A','A')) . snd) portalsOut'
    exit  = fst $ head $ filter ((== ('Z','Z')) . snd) portalsOut'

    portalsOut = filter (\(_,name) -> name /= ('A','A') && name /= ('Z','Z')) portalsOut'

    outSwapped = sort $ fmap (\(x,y) -> (y,x)) portalsOut
    inSwapped = sort $ fmap (\(x,y) -> (y,x)) portalsIn

    portalMap = M.fromList $
                concat $
                zipWith (\(nO,pO) (nI,pI) -> if nO /= nI
                                             then error ""
                                             else [(pO, (pI,Out)), (pI, (pO,In))]) outSwapped inSwapped

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

findHorLeft :: Grid -> M.Map Pos (Char,Char)
findHorLeft (g,gx,gy) =
    M.fromList $
    catMaybes $
    fmap (\[p1,p2,p3] -> do
                           c1 <- M.lookup p1 g
                           guard $ isAsciiUpper c1
                           c2 <- M.lookup p2 g
                           guard $ isAsciiUpper c2
                           c3 <- M.lookup p3 g
                           guard $ c3 == '.'
                           return (p3,(c1,c2))) pss
  where
    pss = [ [(x,y),(x+1,y),(x+2,y)] | y <- [0..gy], x <- [0..gx-2]]

findHorRight :: Grid -> M.Map Pos (Char,Char)
findHorRight (g,gx,gy) =
    M.fromList $
    catMaybes $
    fmap (\[p1,p2,p3] -> do
                           c1 <- M.lookup p1 g
                           guard $ c1 == '.'
                           c2 <- M.lookup p2 g
                           guard $ isAsciiUpper c2
                           c3 <- M.lookup p3 g
                           guard $ isAsciiUpper c3
                           return (p1,(c2,c3))) pss
  where
    pss = [ [(x,y),(x+1,y),(x+2,y)] | y <- [0..gy], x <- [0..gx-2]]

findVerUp :: Grid -> M.Map Pos (Char,Char)
findVerUp (g,gx,gy) =
    M.fromList $
    catMaybes $
    fmap (\[p1,p2,p3] -> do
                           c1 <- M.lookup p1 g
                           guard $ isAsciiUpper c1
                           c2 <- M.lookup p2 g
                           guard $ isAsciiUpper c2
                           c3 <- M.lookup p3 g
                           guard $ c3 == '.'
                           return (p3,(c1,c2))) pss
  where
    pss = [ [(x,y),(x,y+1),(x,y+2)] | y <- [0..gy-2], x <- [0..gx]]

findVerDown :: Grid -> M.Map Pos (Char,Char)
findVerDown (g,gx,gy) =
    M.fromList $
    catMaybes $
    fmap (\[p1,p2,p3] -> do
                           c1 <- M.lookup p1 g
                           guard $ c1 == '.'
                           c2 <- M.lookup p2 g
                           guard $ isAsciiUpper c2
                           c3 <- M.lookup p3 g
                           guard $ isAsciiUpper c3
                           return (p1,(c2,c3))) pss
  where
    pss = [ [(x,y),(x,y+1),(x,y+2)] | y <- [0..gy-2], x <- [0..gx]]

neighs :: Pos -> [Pos]
neighs (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

solve :: Maze -> ([Pos],InfInt)
solve (paths, entry, exit, portal) = go queue dists exps prevs
  where
    source = entry
    dists  = M.singleton source 0
    queue  = PSQ.singleton source 0
    exps   = S.singleton source
    prevs  = M.empty

    go _queue _dists _exps _prevs =
      case PSQ.minView _queue of
        Nothing -> error ""
        Just (_vertex PSQ.:-> _d0, _queue') ->
          if _vertex == exit
          then (unfoldPrevs _vertex _prevs,_d0)
          else let neighbours = filter (\v -> (v `S.notMember` _exps) && (v `S.member` paths)) $ neighs _vertex
                   tele = M.lookup _vertex portal
                   neighbours' = case tele of
                                   Nothing -> neighbours
                                   Just _v' -> if _v' `S.member` _exps
                                               then neighbours
                                               else _v' : neighbours
                   (_queue'',_dists',_prevs') = foldl' (\(_qAcc,_dAcc,_pAcc) _v' ->
                                                           let alt = _d0 + F 1
                                                               _d1 = M.lookup _v' _dists
                                                               d1  = case _d1 of
                                                                       Nothing -> Inf
                                                                       Just i  -> i
                                                           in if alt < d1
                                                              then (PSQ.alter (const $ Just alt) _v' _qAcc,
                                                                    M.insert _v' alt _dAcc,
                                                                    M.insert _v' _vertex _pAcc)
                                                              else (PSQ.alter (const $ Just d1) _v' _qAcc,_dAcc,_pAcc)) (_queue',_dists,_prevs) neighbours'
                   _exps' = S.insert _vertex _exps
               in  go _queue'' _dists' _exps' _prevs'

    unfoldPrevs v _prevs = case M.lookup v _prevs of
                                  Nothing -> [v]
                                  Just v' -> v : unfoldPrevs v' _prevs

solveRec :: RecMaze -> ([(Pos,Int)],InfInt)
solveRec (paths, entry', exit', portal) = go queue dists exps prevs
  where
    entry = (entry',0)
    exit = (exit',0)

    source = entry
    dists  = M.singleton source 0
    queue  = PSQ.singleton source 0
    exps   = S.singleton source
    prevs  = M.empty

    go _queue _dists _exps _prevs =
      case PSQ.minView _queue of
        Nothing -> error ""
        Just (_vertex@(_v,_depth) PSQ.:-> _d0, _queue') ->
          if _vertex == exit
          then (unfoldPrevs _vertex _prevs,_d0)
          else let neighbours = filter (\v -> v `S.notMember` _exps) $ fmap (\v -> (v,_depth)) $ filter (\v -> v `S.member` paths) $ neighs _v
                   tele = M.lookup _v portal
                   neighbours' = case tele of
                                   Nothing -> neighbours
                                   Just (_v',In) -> if (_v',_depth+1) `S.member` _exps
                                                    then neighbours
                                                    else (_v',_depth+1) : neighbours
                                   Just (_v',Out) -> if _depth == 0
                                                     then neighbours
                                                     else if (_v',_depth-1) `S.member` _exps
                                                          then neighbours
                                                          else (_v',_depth-1) : neighbours
                   (_queue'',_dists',_prevs') = foldl' (\(_qAcc,_dAcc,_pAcc) _v' ->
                                                           let alt = _d0 + F 1
                                                               _d1 = M.lookup _v' _dists
                                                               d1  = case _d1 of
                                                                       Nothing -> Inf
                                                                       Just i  -> i
                                                           in if alt < d1
                                                              then (PSQ.alter (const $ Just alt) _v' _qAcc,
                                                                    M.insert _v' alt _dAcc,
                                                                    M.insert _v' _vertex _pAcc)
                                                              else (PSQ.alter (const $ Just d1) _v' _qAcc,_dAcc,_pAcc)) (_queue',_dists,_prevs) neighbours'
                   _exps' = S.insert _vertex _exps
               in  go _queue'' _dists' _exps' _prevs'

    unfoldPrevs v _prevs = case M.lookup v _prevs of
                                  Nothing -> [v]
                                  Just v' -> v : unfoldPrevs v' _prevs

fileName :: String
fileName = "data/a20/input.txt"

a20_input :: IO String
a20_input = readFile fileName

a20_ans1 :: String -> Int
a20_ans1 i = (\x -> case x of
                      Inf -> -1
                      F y -> y) l
  where
    maze = parseMaze i
    (path,l) = solve maze

a20_ans2 :: String -> Int
a20_ans2 i = (\x -> case x of
                      Inf -> -1
                      F y -> y) l
  where
    maze@(_,_,_,ps) = parseRecMaze i
    (path,l) = solveRec maze
