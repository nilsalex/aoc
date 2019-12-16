module A07 (a07_input, a07_ans1, a07_ans2) where

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

op :: Int -> Programme -> Op
op pos (Programme xs')
    | o == 1 = Add p1 p2 p3 b1 b2 b3
    | o == 2 = Mult p1 p2 p3 b1 b2 b3
    | o == 3 = Get p1'' b1
    | o == 4 = Put p1'' b1
    | o == 5 = JumpIf p1' p2' b1 b2
    | o == 6 = JumpIfNot p1' p2' b1 b2
    | o == 7 = Less p1 p2 p3 b1 b2 b3
    | o == 8 = Equals p1 p2 p3 b1 b2 b3
    | o == 99 = Stop
    | otherwise = error "Unknown op code"
  where
    (x:xs) = drop pos xs'
    m1 = x `div` 10000
    m2 = (x `mod` 10000) `div` 1000
    m3 = ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    [b3, b2, b1] = fmap b [m1, m2, m3]
    [p1, p2, p3] = take 3 xs
    [p1', p2'] = take 2 xs
    p1'' = head xs

    b i = i /= 0

newtype Programme = Programme { fromProgramme :: [Int] } deriving Show
newtype Buffer    = Buffer { fromBuffer :: [Int] } deriving Show
data Code      = RUN | HALT deriving Show
type ProgState = (Code, Int, Programme, Buffer, Buffer)
type ArrayState = [ProgState]

arrayState :: [Int] -> Programme -> ArrayState
arrayState phases prog =
  case fmap (\phase -> (RUN, 0, prog, Buffer [phase], Buffer [])) phases of
    (c,i,p,Buffer is,os):xs ->
      (c,i,p,Buffer (is ++ [0]),os):xs

carryOutputs :: ArrayState -> ArrayState
carryOutputs ss = zipWith (\(c,i,p,ib,ob) ob' -> (c,i,p,merge ob' ib,Buffer [])) ss outputs
  where
    outputs = (\x -> last x : init x) $ fmap (\(_,_,_,_,b) -> b) ss
    merge (Buffer os) (Buffer is) = Buffer $ reverse os ++ is

{-
updateLastOut :: State ArrayState ()
updateLastOut =
  do
   (_, ps) <- getS
   case last ps of
     (c,i,p,is,Buffer (o:os)) ->
       putS (o, ps)
     _ -> return ()
-}

evalArray :: State ArrayState ()
evalArray =
  do
   modifyS carryOutputs
   modifyS $ fmap (snd . runState eval)
   s <- getS
   case all (\(c,_,_,_,_) -> case c of
                               HALT -> True
                               _    -> False) s of
      True -> return ()
      False -> evalArray

evalArrayIO :: ArrayState -> IO ()
evalArrayIO s =
  do
    mapM_ print s
    putStrLn ""
    l <- getLine
    case l of
      'c':_ -> evalArrayIO (carryOutputs s)
      _  -> evalArrayIO $ fmap (snd . runState eval) s

eval :: State ProgState ()
eval =
  do
     (code, ip, prog, istack, ostack) <- getS
     case code of
       HALT -> return ()
       _    -> 
         let o = op ip prog in
         case o of
           Stop ->
             putS (HALT, ip, prog, istack, ostack)
           Put a ma ->
             putS (RUN, ip + 2, prog, istack, Buffer $ look a ma prog : fromBuffer ostack)
           Get a ma ->
             case fromBuffer istack of
               [] -> putS (RUN, ip, prog, istack, ostack)
               (i:is) -> putS (RUN, ip + 2, write a ma i prog, Buffer is, ostack)
           Add a b c ma mb mc ->
             putS (RUN, ip + 4, write c mc (look a ma prog + look b mb prog) prog, istack, ostack)
           Mult a b c ma mb mc ->
             putS (RUN, ip + 4, write c mc (look a ma prog * look b mb prog) prog, istack, ostack)
           Less a b c ma mb mc ->
             putS (RUN, ip + 4, write c mc (i $ look a ma prog < look b mb prog) prog, istack, ostack)
           Equals a b c ma mb mc ->
             putS (RUN, ip + 4, write c mc (i $ look a ma prog == look b mb prog) prog, istack, ostack)
           JumpIf a b ma mb ->
             case toB $ look a ma prog of
               True -> putS (RUN, look b mb prog, prog, istack, ostack)
               False -> putS (RUN, ip + 3, prog, istack, ostack)
           JumpIfNot a b ma mb ->
             case toB $ look a ma prog of
               False -> putS (RUN, look b mb prog, prog, istack, ostack)
               True -> putS (RUN, ip + 3, prog, istack, ostack)

  where

    look i mi (Programme xs) = case mi of
                                 True -> i
                                 False -> xs !! i

    write i mi v (Programme xs) = Programme $
                                  case mi of
                                    True -> error ""
                                    False -> modify i (const v) xs
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
fileName = "data/a07/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a07_input :: IO Programme
a07_input = loadProgramme fileName

a07_ans1 :: Programme -> Int
a07_ans1 prog = maximum scores
  where
    ps = perms 0
    scores = fmap (\p -> let s0 = arrayState p prog
                             s1 = snd $ runState evalArray $ s0
                         in case s1 of
                              (_,_,_,Buffer (i:_),_):_ -> i) ps

a07_ans2 :: Programme -> Int
a07_ans2 prog = maximum scores
  where
    ps = perms 5
    scores = fmap (\p -> let s0 = arrayState p prog
                             s1 = snd $ runState evalArray $ s0
                         in case s1 of
                              (_,_,_,Buffer (i:_),_):_ -> i) ps

perms :: Int -> [[Int]]
perms i = [[a,b,c,d,e] | a <- [i..4+i], b <- [i..4+i], c <- [i..4+i], d <- [i..4+i], e <- [i..4+i], a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]
