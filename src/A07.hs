module A07 (a07_input, a07_ans1) where

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
                                
data Op = Add Int Int Int Bool Bool Bool |
          Mult Int Int Int Bool Bool Bool |
          Get Int Bool |
          Put Int Bool |
          JumpIf Int Int Bool Bool |
          JumpIfNot Int Int Bool Bool |
          Less Int Int Int Bool Bool Bool |
          Equals Int Int Int Bool Bool Bool |
          Stop

add :: Int
add = 1

mult :: Int
mult = 2

get :: Int
get = 3

put :: Int
put = 4

jumpIf :: Int
jumpIf = 5

jumpIfNot :: Int
jumpIfNot = 6

less :: Int
less = 7

equals :: Int
equals = 8

stop :: Int
stop = 99

op :: Int -> [Int] -> Op
op pos xs'
    | o == add = Add p1 p2 p3 b1 b2 b3
    | o == mult = Mult p1 p2 p3 b1 b2 b3
    | o == get = Get p1 b1
    | o == put = Put p1 b1
    | o == jumpIf = JumpIf p1 p2 b1 b2
    | o == jumpIfNot = JumpIfNot p1 p2 b1 b2
    | o == less = Less p1 p2 p3 b1 b2 b3
    | o == equals = Equals p1 p2 p3 b1 b2 b3
    | o == stop = Stop
  where
    (x:xs) = drop pos xs'
    m1 = x `div` 10000
    m2 = (x `mod` 10000) `div` 1000
    m3 = ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    [b3, b2, b1] = fmap b [m1, m2, m3]
    [p1, p2, p3] = take 3 xs

    b i = i /= 0

type ProgState = (Int, [Int], [Int], [Int]) -- instruction pointer,
                                            -- instruction stack,
                                            -- input stack,
                                            -- output stack

run :: [Int] -> [Int] -> ProgState
run prog istack = snd $ (runState eval) state0
  where
    state0 = (0, prog, istack, [])

eval :: State ProgState ()
eval = do
         (ip, prog, istack, ostack) <- getS
         let o = op ip prog
         case o of
           Stop ->
             return ()
           Put a ma ->
             putS (ip + 2, prog, istack, look a ma prog : ostack) >> eval
           Get a ma ->
             putS (ip + 2, write a ma (head istack) prog, tail istack, ostack) >> eval
           Add a b c ma mb mc ->
             putS (ip + 4, write c mc (look a ma prog + look b mb prog) prog, istack, ostack) >> eval
           Mult a b c ma mb mc ->
             putS (ip + 4, write c mc (look a ma prog * look b mb prog) prog, istack, ostack) >> eval
           Less a b c ma mb mc ->
             putS (ip + 4, write c mc (i $ look a ma prog < look b mb prog) prog, istack, ostack) >> eval
           Equals a b c ma mb mc ->
             putS (ip + 4, write c mc (i $ look a ma prog == look b mb prog) prog, istack, ostack) >> eval
           JumpIf a b ma mb ->
             case toB $ look a ma prog of
               True -> putS (look b mb prog, prog, istack, ostack) >> eval
               False -> putS (ip + 3, prog, istack, ostack) >> eval
           JumpIfNot a b ma mb ->
             case toB $ look a ma prog of
               False -> putS (look b mb prog, prog, istack, ostack) >> eval
               True -> putS (ip + 3, prog, istack, ostack) >> eval

  where

    look i mi xs = case mi of
                     True -> i
                     False -> xs !! i

    write i mi v xs = case mi of
                        True -> error ""
                        False -> modify i (const v) xs
    i False = 0
    i True  = 1
    toB i = i == 1

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

ampSeq :: [Int] -> [Int] -> Int
ampSeq (p:ps) prog = ampSeq' (p:ps) 0
  where
    ampSeq' [] x = x
    ampSeq' (q:qs) x = case run prog [q,x] of
                         (_, _, _, o:[]) -> ampSeq' qs o

fileName :: String
fileName = "data/a07/input.txt"

convProgramme :: String -> [Int]
convProgramme = fmap read . splitAt' ','

loadProgramme :: String -> IO [Int]
loadProgramme = fmap convProgramme . readFile

a07_input :: IO [Int]
a07_input = loadProgramme fileName

a07_ans1 :: [Int] -> Int
a07_ans1 prog = maximum res
  where
    ps = [[a,b,c,d,e] | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4],
                        a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]
    res = fmap (\p -> ampSeq p prog) ps
