module A15 (a15_input) where

import Debug.Trace

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
dirs E = [N,W,E]

data Tree a = Node a [Tree a]

gameTrees :: [Tree Dir]
gameTrees = gameTrees' [N,S,W,E]
  where
    gameTrees' xs = fmap (\x -> Node x $ gameTrees' (dirs x)) xs

branch :: [Tree a] -> [a]
branch [] = error " "
branch ((Node n []):ts) = error ""
branch ((Node n ts):_) = n : branch ts

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
               _:_:[] -> putS (OUT, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> return OUT
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
fileName = "data/a15/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . fmap read . splitAt' ','

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

