module A17 (a17_input,a17_ans1,a17_ans2) where

import Data.Char (chr,ord)
import Data.List (foldl',tails,intersperse)

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
op pos (Programme xs')
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
    (x:xs) = drop pos xs'
    m3 = mode $ x `div` 10000
    m2 = mode $ (x `mod` 10000) `div` 1000
    m1 = mode $ ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    [p1, p2, p3] = take 3 xs
    [p1', p2'] = take 2 xs
    p1'' = head xs

    b i = i /= 0

newtype Programme = Programme { fromProgramme :: [Int] } deriving Show
newtype Buffer    = Buffer { fromBuffer :: [Int] } deriving Show
data Code      = RUN | WAIT | OUT | HALT deriving Show
type ProgState = (Code, Int, Int, Programme, Buffer, Buffer)

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
                                   POS -> xs !! i
                                   PAR -> i
                                   REL -> xs !! (b + i)

    write i mi b v (Programme xs) = Programme $
                                      case mi of
                                        POS -> modify i (const v) xs
                                        REL -> modify (b + i) (const v) xs
                                        PAR -> error "cannot write in parameter mode"

    i False = 0
    i True  = 1
    toB i = i /= 0

modify :: Int -> (a -> a) -> [a] -> [a]
modify i f [] = []
modify 0 f (x:xs) = f x : xs
modify i f (x:xs) = x : (modify (i-1) f xs)

splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' d [] = []
splitAt' d xs = case word of
                  [] -> splitAt' d xs
                  _  -> word : splitAt' d xs'
  where
    (word, xs') = (takeWhile (/= d) xs, dropWhile (==d) $ dropWhile (/= d) xs)

fileName :: String
fileName = "data/a17/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a17_input :: IO Programme
a17_input = loadProgramme fileName

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

toMap :: [Char] -> [(Int,Int)]
toMap xs = foldl' (\acc (x,y) -> case (ls !! y) !! x of
                                   '#' -> insert' (x,y) acc
                                   _   -> acc)
                  [] $ (,) <$> [0..xDim-1] <*> [0..yDim-1]
  where
    ls = filter (not . null) $ lines xs
    xDim = length $ head ls
    yDim = length ls

isIntersection :: [(Int,Int)] -> (Int,Int) -> Bool
isIntersection ms (x,y) = all (`elem'` ms) poss
  where
    poss = [(x,y),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

data Dir = U | D | L | R deriving Show

next :: Dir -> (Dir,Dir)
next U = (L,R)
next D = (R,L)
next L = (D,U)
next R = (U,D)

move :: Dir -> (Int,Int) -> (Int,Int)
move U (x,y) = (x,y-1)
move D (x,y) = (x,y+1)
move L (x,y) = (x-1,y)
move R (x,y) = (x+1,y)

moves :: [(Int,Int)] -> Dir -> (Int,Int) -> (Int,(Int,Int))
moves ms d p
    | p' `elem'` ms = (1+n,p'')
    | otherwise = (0,p)
  where
    p' = move d p
    (n,p'') = moves ms d p'

toSeq :: [(Int,Int)] -> (Int,Int) -> Dir -> [String]
toSeq ms (x,y) d
    | nl > 0 = ('L' : show nl) : toSeq ms pl leftDir
    | nr > 0 = ('R' : show nr) : toSeq ms pr rightDir
    | otherwise = []
  where
    (leftDir, rightDir) = next d
    ((nl, pl), (nr, pr)) = (moves ms leftDir (x,y), moves ms rightDir (x,y))


a17_ans1 :: Programme -> Int
a17_ans1 (Programme prog) = sum $ fmap (\(a,b) -> a*b) intersections
  where
    s0 = (RUN,0,0,Programme (prog ++ take 10000 (repeat 0)),Buffer [], Buffer [])
    (Buffer os, _) = runState (eval >> readOBuf) s0
    ms = fmap chr $ reverse os
    m = toMap ms
    intersections = filter (isIntersection m) m

a17_ans2 :: Programme -> Int
a17_ans2 (Programme (x:prog)) = head os
  where
    s = getSeq (Programme (x:prog))
    mainSeq = "ABACBCBCAC"
    seqA = ["R","12","L","6","R","12"]
    seqB = ["L","8","L","6","L","10"]
    seqC = ["R","12","L","10","L","6","R","10"]
    no = "n"

    mainSeq' = intersperse ',' mainSeq
    seqA' = concat $ intersperse "," seqA
    seqB' = concat $ intersperse "," seqB
    seqC' = concat $ intersperse "," seqC

    input = concat $ fmap (++"\n") [mainSeq',seqA',seqB',seqC',no]

    s0 = (RUN,0,0,Programme $ (2:prog) ++ take 10000 (repeat 0),Buffer (fmap ord input),Buffer [])
    (Buffer os,sFinal) = runState (eval >> readOBuf) s0

getSeq :: Programme -> [String]
getSeq (Programme prog) = s
  where
    s0 = (RUN,0,0,Programme (prog ++ take 10000 (repeat 0)),Buffer [], Buffer [])
    (Buffer os, _) = runState (eval >> readOBuf) s0
    ms = fmap chr $ reverse os
    m = toMap ms
    ls = filter (not . null) $ lines ms
    startPos = head $ filter (\(x,y) -> not $ ((ls !! y) !! x) `elem'` "#.") $ (,) <$> [0..length (head ls) - 1] <*> [0..length ls - 1]
    s = toSeq m startPos U

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort xl ++ [x] ++ sort xr
  where
    xl = filter (<=x) xs
    xr = filter (>x) xs

count :: Eq a => [a] -> [a] -> Int
count [] _ = error ""
count sub xs = scan sub xs'
  where
    xs' = dropWhile (/= head sub) xs

    scan _ [] = 0
    scan [] ys = 1 + count sub ys
    scan (y:ys) (z:zs)
      | y == z = scan ys zs
      | otherwise = count sub zs

subLists :: [a] -> [[a]]
subLists xs = [take b (drop a xs) | a <- [0..l-1], b <- [1..l-a]]
  where
    l = length xs

compress :: (Eq a, Ord a) => [a] -> ([a], [Either [a] ()])
compress xs = (chunk, substitute chunk xs)
  where
    chunks = subLists xs
    counts = 
      sort $
      filter (\(_,c,_) -> c == 4) $
      fmap (\sub -> let len = length sub
                        c = count sub xs
                    in (len,c,sub)) chunks
    chunk = (\(_,_,s) -> s) $ last counts

substitute :: Eq a => [a] -> [a] -> [Either [a] ()]
substitute cs xs = go [] xs
  where
    l = length cs

    go ls [] = case ls of
                 [] -> []
                 _  -> [Left ls]
    go ls rs@(r:rs') = if take l rs == cs
                       then case ls of
                             [] -> Right () : go [] (drop l rs)
                             _  -> Left ls : Right () : go [] (drop l rs)
                       else go (ls ++ [r]) rs'
