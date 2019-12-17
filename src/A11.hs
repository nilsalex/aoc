module A11 (a11_input,a11_ans1,a11_ans2) where

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
data Code      = RUN | WAIT | HALT deriving Show
type ProgState = (Code, Int, Int, Programme, Buffer, Buffer)
newtype Pos = Pos { fromPos :: (Int, Int) } deriving (Show, Ord, Eq)
data Dir = U | D | L | R deriving Show
data Color = White | Black deriving Show
type RoboState = (Pos, Dir, [(Pos, Color)], ProgState)

colToInt :: Color -> Int
colToInt Black = 0
colToInt White = 1

intToCol :: Int -> Color
intToCol 0 = Black
intToCol 1 = White

move :: Dir -> Pos -> Pos
move U (Pos (x,y)) = Pos (x,y+1)
move D (Pos (x,y)) = Pos (x,y-1)
move L (Pos (x,y)) = Pos (x-1,y)
move R (Pos (x,y)) = Pos (x+1,y)

turn :: Int -> Dir -> Dir
turn 0 U = L
turn 0 L = D
turn 0 D = R
turn 0 R = U
turn 1 U = R
turn 1 R = D
turn 1 D = L
turn 1 L = U

updateCol :: Pos -> Color -> [(Pos, Color)] -> [(Pos, Color)]
updateCol p c [] = [(p, c)]
updateCol p c ((p',c'):xs)
  | p < p' = (p,c):(p',c'):xs
  | p == p' = (p,c):xs
  | p > p' = (p',c') : updateCol p c xs

lookupCol :: Pos -> [(Pos, Color)] -> Color
lookupCol _ [] = Black
lookupCol p ((p',c):xs)
  | p < p' = Black
  | p == p' = c
  | p > p' = lookupCol p xs

roboState :: ProgState -> RoboState
roboState prog = (Pos (0,0), U, [], prog)

roboState' :: ProgState -> RoboState
roboState' prog = (Pos (0,0), U, [(Pos (0,0), White)], prog)

progState :: Programme -> ProgState
progState prog = (RUN, 0, 0, prog, Buffer [], Buffer [])

appendIBuf :: Int -> State ProgState ()
appendIBuf i = modifyS (\(x1,x2,x3,x4,Buffer is,x5) -> (x1,x2,x3,x4,Buffer (i:is),x5))

updateOBuf :: [Int] -> State ProgState ()
updateOBuf os = modifyS (\(x1,x2,x3,x4,x5,_) -> (x1,x2,x3,x4,x5,Buffer os))

evalRobo :: State RoboState Int
evalRobo =
  do
    (pos, dir, colmap, prog) <- getS
    let curCol = lookupCol pos colmap
    let prog'@(code,_,_,_,_,Buffer os) = snd $ runState
                                         (appendIBuf (colToInt curCol) >> evalRec)
                                         prog
    case os of
      o2:o1:os' -> do
                    let colmap' = updateCol pos (intToCol o1) colmap
                    let dir' = turn o2 dir
                    let pos' = move dir' pos
                    let prog'' = snd $ runState (updateOBuf os') prog'
                    putS (pos', dir', colmap', prog'')
                    case code of
                      HALT -> return $ length $ colmap
                      WAIT -> evalRobo
                      _    -> error "undefined state"
      _ -> error $ show prog'

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
             putS (RUN, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> return RUN
           Get a ma ->
             case fromBuffer istack of
               [] -> putS (WAIT, ip, bp, prog, istack, ostack) >> return WAIT
               (i:is) -> putS (RUN, ip + 2, bp, write a ma bp i prog, Buffer is, ostack) >> return RUN
           Add a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (look a ma bp prog + look b mb bp prog) prog, istack, ostack) >> return RUN
           Mult a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (look a ma bp prog * look b mb bp prog) prog, istack, ostack) >> return RUN
           Less a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog < look b mb bp prog) prog, istack, ostack) >> return RUN
           Equals a b c ma mb mc ->
             putS (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog == look b mb bp prog) prog, istack, ostack) >> return RUN
           JumpIf a b ma mb ->
             case toB $ look a ma bp prog of
               True -> putS (RUN, look b mb bp prog, bp, prog, istack, ostack) >> return RUN
               False -> putS (RUN, ip + 3, bp, prog, istack, ostack) >> return RUN
           JumpIfNot a b ma mb ->
             case toB $ look a ma bp prog of
               False -> putS (RUN, look b mb bp prog, bp, prog, istack, ostack) >> return RUN
               True -> putS (RUN, ip + 3, bp, prog, istack, ostack) >> return RUN
           Base a ma ->
             putS (RUN, ip + 2, bp + look a ma bp prog, prog, istack, ostack) >> return RUN

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
fileName = "data/a11/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . (++ take 10000 (repeat 0)) . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a11_input :: IO Programme
a11_input = loadProgramme fileName

evalRec :: State ProgState ()
evalRec = do
           c <- eval
           case c of
             HALT -> return ()
             WAIT -> return ()
             _    -> evalRec

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' n [] = []
chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

a11_ans1 :: Programme -> Int
a11_ans1 prog = fst $ runState evalRobo $ rs0
  where
    ps0 = progState prog
    rs0 = roboState ps0

a11_ans2 :: Programme -> String
a11_ans2 prog = unlines $ reverse chars
  where
    ps0 = progState prog
    rs0 = roboState' ps0
    (_,_,cmap,_) = snd $ runState evalRobo $ rs0
    minX = minimum $ fmap (\(Pos (x,_),_) -> x) cmap
    maxX = maximum $ fmap (\(Pos (x,_),_) -> x) cmap
    minY = minimum $ fmap (\(Pos (_,y),_) -> y) cmap
    maxY = maximum $ fmap (\(Pos (_,y),_) -> y) cmap
    colors = fmap (\y -> fmap (\x -> lookupCol (Pos (x,y)) cmap) [minX..maxX]) [minY..maxY]
    chars = fmap (fmap (\c -> case c of
                               Black -> '\x2B1B'
                               White -> '\x2B1C')) colors
