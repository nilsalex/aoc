module A15 (a15_input,a15_ans1,a15_ans2) where

import qualified Data.Sequence as S

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
                                
data Op = Add Int Int Int Mode Mode Mode |
          Mult Int Int Int Mode Mode Mode |
          Get Int Mode |
          Put Int Mode |
          JumpIf Int Int Mode Mode |
          JumpIfNot Int Int Mode Mode |
          Less Int Int Int Mode Mode Mode |
          Equals Int Int Int Mode Mode Mode |
          Base Int Mode |
          Stop

data Mode = POS | PAR | REL deriving Show

mode :: Int -> Mode
mode 0 = POS
mode 1 = PAR
mode 2 = REL

op :: Int -> Programme -> Op
op pos (Programme xs)
    | o == 1 = Add p1 p2 p3 m1 m2 m3
    | o == 2 = Mult p1 p2 p3 m1 m2 m3
    | o == 3 = Get p1'' m1
    | o == 4 = Put p1'' m1
    | o == 5 = JumpIf p1' p2' m1 m2
    | o == 6 = JumpIfNot p1' p2' m1 m2
    | o == 7 = Less p1 p2 p3 m1 m2 m3
    | o == 8 = Equals p1 p2 p3 m1 m2 m3
    | o == 9 = Base p1 m1
    | o == 99 = Stop
    | otherwise = error "Unknown op code"
  where
    x = S.index xs pos
    m3 = mode $ x `div` 10000
    m2 = mode $ (x `mod` 10000) `div` 1000
    m1 = mode $ ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    p1 = S.index xs (pos+1)
    p2 = S.index xs (pos+2)
    p3 = S.index xs (pos+3)

    p1' = S.index xs (pos+1)
    p2' = S.index xs (pos+2)

    p1'' = S.index xs (pos+1)

    b i = i /= 0

newtype Programme = Programme { fromProgramme :: S.Seq Int } deriving Show
newtype Buffer    = Buffer { fromBuffer :: [Int] } deriving Show
data Code      = RUN | WAIT | OUT | HALT deriving Show
type ProgState = (Code, Int, Int, Programme, Buffer, Buffer)
data Dir = N | S | W | E deriving (Show, Eq, Ord)

dirToInt :: Dir -> Int
dirToInt N = 1
dirToInt S = 2
dirToInt W = 3
dirToInt E = 4

intToDir :: Int -> Dir
intToDir 1 = N
intToDir 2 = S
intToDir 3 = W
intToDir 4 = E

dirs :: Dir -> [Dir]
dirs N = [N,W,E]
dirs S = [S,W,E]
dirs W = [N,S,W]
dirs E = [N,S,E]

walk :: Dir -> Pos -> Pos
walk N (x,y) = (x,y+1)
walk S (x,y) = (x,y-1)
walk W (x,y) = (x-1,y)
walk E (x,y) = (x+1,y)

data Tree a = Node a [Tree a] deriving Show
data EvalTree a b = EvalNode a [EvalTree a b] | Leaf b deriving Show
type Pos = (Int,Int)

dirTrees :: [Tree Dir]
dirTrees = dirTrees' [N,S,W,E]
  where
    dirTrees' xs = fmap (\x -> Node x $ dirTrees' (dirs x)) xs

mapTree :: Pos -> ProgState -> Tree Dir -> EvalTree Pos Pos
mapTree pos prog (Node d ts) =
    case code of
      HALT -> error ""
      RUN  -> error ""
      OUT  -> error ""
      WAIT -> case os of
                []  -> error ""
                0:_ -> EvalNode pos' []
                1:_ -> EvalNode pos' $ fmap (mapTree pos' prog') ts
                2:_ -> Leaf pos'
  where
    prog'@(code,_,_,_,_,Buffer os) = snd $ runState (appendIBuf (dirToInt d) >> eval) prog
    pos' = walk d pos

gameTree :: Int -> ProgState -> Tree Dir -> EvalTree ProgState Int
gameTree depth prog (Node d ts) = 
    case code of
      HALT -> error ""
      RUN  -> error ""
      OUT  -> error ""
      WAIT -> case os of
                []  -> error $ show depth
                0:_ -> EvalNode prog' []
                1:_ -> EvalNode prog' $ fmap (gameTree (depth+1) prog') ts
                2:_ -> Leaf (depth+1)
  where
    prog'@(code,_,_,_,_,Buffer os) = snd $ runState (appendIBuf (dirToInt d) >> eval) prog

mapList :: [EvalTree Pos a] -> [Pos]
mapList [] = []
mapList ts = concat $ fmap mapList' ts
  where
    mapList' (Leaf _) = []
    mapList' (EvalNode _ []) = []
    mapList' (EvalNode n ts') = n : mapList ts'

toMap :: [EvalTree Pos Pos] -> ([Pos],Pos)
toMap ts = (l', o2)
  where
    l = mapList ts
    l' = foldr insert' [(0,0)] l

    Just o2 = head $ filter (\x -> case x of
                                    Nothing -> False
                                    _ -> True) $ fmap leaf ts

    leaf (Leaf p) = Just p
    leaf (EvalNode _ []) = Nothing
    leaf (EvalNode n (x:xs)) = case leaf x of
                                  Nothing -> leaf (EvalNode n xs)
                                  Just p -> Just p

showMap :: ([Pos],Pos) -> [String]
showMap (ps,o) = str
  where
    minX = minimum $ fmap fst ps
    minY = minimum $ fmap snd ps
    maxX = maximum $ fmap fst ps
    maxY = maximum $ fmap snd ps

    str = fmap (\y -> fmap (\x -> if (x,y) `elem'` ps
                                  then ' '
                                  else if (x,y) == o
                                       then 'O'
                                       else '#') [minX..maxX]) [minY..maxY]

branch :: [Tree a] -> [a]
branch [] = error " "
branch ((Node n []):ts) = error ""
branch ((Node n ts):_) = n : branch ts

leafs :: [EvalTree a b] -> [b]
leafs [] = []
leafs (x:xs) = leafs' x ++ leafs xs
  where
    leafs' (Leaf l) = [l]
    leafs' (EvalNode n ts) = leafs ts

del :: Int -> [Tree a] -> [Tree a]
del 0 [] = error ""
del 0 (t:ts) = ts
del i ((Node n []):ts) = del i ts
del i ((Node n ts'):ts) = case del (i-1) ts' of
                            [] -> ts
                            ts'' -> (Node n ts''):ts

appendIBuf :: Int -> State ProgState ()
appendIBuf i = modifyS (\(x1,x2,x3,x4,Buffer is,x5) -> (x1,x2,x3,x4,Buffer (i:is),x5))

readOBuf :: State ProgState Buffer
readOBuf = do
            (x1,x2,x3,x4,x5,os) <- getS
            putS (x1,x2,x3,x4,x5,Buffer [])
            return os

eval :: State ProgState Code
eval =
  do
     (code, ip, bp, prog, istack, ostack) <- getS
     case code of
       HALT -> return HALT
       _    -> 
         let o = op ip prog in
         case o of
           Stop ->
             putS (HALT, ip, bp, prog, istack, ostack) >> return HALT
           Put a ma ->
             case fromBuffer ostack of
               -- _:_:[] -> putS (OUT, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> return OUT
               _      -> putS (RUN, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> eval
           Get a ma ->
             case fromBuffer istack of
               [] -> putS (WAIT, ip, bp, prog, istack, ostack) >> return WAIT
               (i:is) -> putS (RUN, ip + 2, bp, write a ma bp i prog, Buffer is, ostack) >> eval
           Add a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (look a ma bp prog + look b mb bp prog) prog, istack, ostack) >> eval
           Mult a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (look a ma bp prog * look b mb bp prog) prog, istack, ostack) >> eval
           Less a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog < look b mb bp prog) prog, istack, ostack) >> eval
           Equals a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog == look b mb bp prog) prog, istack, ostack) >> eval
           JumpIf a b ma mb ->
             case toB $ look a ma bp prog of
               True -> putS (RUN, look b mb bp prog, bp, prog, istack, ostack) >> eval
               False -> putS (RUN, ip + 3, bp, prog, istack, ostack) >> eval
           JumpIfNot a b ma mb ->
             case toB $ look a ma bp prog of
               False -> putS (RUN, look b mb bp prog, bp, prog, istack, ostack) >> eval
               True -> putS (RUN, ip + 3, bp, prog, istack, ostack) >> eval
           Base a ma ->
             putS (RUN, ip + 2, bp + look a ma bp prog, prog, istack, ostack) >> eval

  where

    look i mi b (Programme xs) = case mi of
                                   POS -> S.index xs i
                                   PAR -> i
                                   REL -> S.index xs (b + i)

    write i mi b v (Programme xs) = Programme $
                                      case mi of
                                        POS -> S.update i v xs
                                        REL -> S.update (b + i) v xs
                                        PAR -> error "cannot write in parameter mode"

    i False = 0
    i True  = 1
    toB i = i /= 0

splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' d [] = []
splitAt' d xs = case word of
                  [] -> splitAt' d xs
                  _  -> word : splitAt' d xs'
  where
    (word, xs') = (takeWhile (/= d) xs, dropWhile (==d) $ dropWhile (/= d) xs)

fileName :: String
fileName = "data/a15/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . S.fromList . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a15_input :: IO Programme
a15_input = loadProgramme fileName

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

remove' :: Ord a => a -> [a] -> [a]
remove' _ [] = []
remove' x (y:ys)
  | x < y = y:ys
  | x == y = ys
  | x > y = y : remove' x ys

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
  | x < y = x:y:ys
  | x == y = y:ys
  | x > y = y : insert' x ys

elem' :: Ord a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
  | x < y = False
  | x == y = True
  | x > y = elem' x ys

fillO2 :: [Pos] -> Pos -> (Int,Int,Int,Int) -> Int
fillO2 posMap o2Init bounds@(minX,maxX,minY,maxY) = go 0 [o2Init]
  where
    go depth o2Map =
      let next' = (\o2 d -> let o2' = walk d o2
                                inB = inBounds o2'
                                isWall = not $ o2' `elem'` posMap
                                isO2 = o2' `elem'` o2Map
                            in if inB && not isWall && not isO2
                               then Just o2'
                               else Nothing) <$> o2Map <*> [N,S,W,E]
          next = fmap (\(Just x) -> x) $ filter (\x -> case x of
                                                         Nothing -> False
                                                         _ -> True) next'
      in case next of
           [] -> depth
           _  -> go (depth+1) $ foldr insert' o2Map next

    inBounds (x,y) = x >= minX && x <= maxX && y >= minY && y <= maxY

a15_ans1 :: Programme -> Int
a15_ans1 prog = minimum $ leafs gts
  where
    s0 = (RUN,0,0,prog,Buffer [],Buffer [])
    ts = dirTrees
    gts = fmap (gameTree 0 s0) ts

a15_ans2 :: Programme -> Int
a15_ans2 prog = fillO2 ps o2 bounds
  where
    s0 = (RUN,0,0,prog,Buffer [],Buffer [])
    ts = dirTrees
    mts = fmap (mapTree (0,0) s0) ts
    (ps,o2) = toMap mts

    minX = minimum $ fmap fst ps
    minY = minimum $ fmap snd ps
    maxX = maximum $ fmap fst ps
    maxY = maximum $ fmap snd ps
    bounds = (minX,maxX,minY,maxY)
