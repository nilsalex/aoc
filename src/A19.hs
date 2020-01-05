{-# LANGUAGE BangPatterns #-}

module A19 (a19_input,a19_ans1,a19_ans2) where

import Data.List (foldl',sort)
import qualified Data.Sequence as SQ

import qualified Data.Set as S
import Data.Maybe (catMaybes)

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
    x = xs' `SQ.index` pos
    m3 = mode $ x `div` 10000
    m2 = mode $ (x `mod` 10000) `div` 1000
    m1 = mode $ ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    p1 = xs' `SQ.index` (pos+1)
    p2 = xs' `SQ.index` (pos+2)
    p3 = xs' `SQ.index` (pos+3)

    p1' = xs' `SQ.index` (pos+1)
    p2' = xs' `SQ.index` (pos+2)

    p1'' = xs' `SQ.index` (pos+1)

    b i = i /= 0

newtype Programme = Programme { fromProgramme :: SQ.Seq Int } deriving Show
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
                                   POS -> xs `SQ.index` i
                                   PAR -> i
                                   REL -> xs `SQ.index` (b + i)

    write i mi b v (Programme xs) = Programme $
                                      case mi of
                                        POS -> SQ.adjust (const v) i xs
                                        REL -> SQ.adjust (const v) (b+i) xs
                                        PAR -> error "cannot write in parameter mode"

    i False = 0
    i True  = 1
    toB i = i /= 0

modify :: Int -> (a -> a) -> [a] -> [a]
modify i f [] = []
modify 0 f (x:xs) = let x' = f x
                    in  x' : xs
modify i f (x:xs) = let xs' = modify (i-1) f xs
                    in  x : xs'

splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' d [] = []
splitAt' d xs = case word of
                  [] -> splitAt' d xs
                  _  -> word : splitAt' d xs'
  where
    (word, xs') = (takeWhile (/= d) xs, dropWhile (==d) $ dropWhile (/= d) xs)

fileName :: String
fileName = "data/a19/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . SQ.fromList . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a19_input :: IO Programme
a19_input = loadProgramme fileName

a19_ans1 :: Programme -> Int
a19_ans1 (Programme prog) = length $ filter (==1) $ (\x y -> res (s0 x y)) <$> [0..49] <*> [0..49]
  where
    prog' = Programme $ prog SQ.>< SQ.replicate 100 0
    s0 x y = (RUN,0,0,prog',Buffer [x,y],Buffer [])
    res s = let (HALT,_,_,_,_,Buffer ob) = snd $ runState eval $ s
            in head ob

a19_ans2 :: Programme -> Int
a19_ans2 (Programme prog) = trace (show $ fit 1 1 0 0) $
                            (\(x,y) -> 10000*x + y) $ case ress of
                                                        [] -> error ""
                                                        r:_ -> r
  where
    prog' = Programme $ prog SQ.>< SQ.replicate 100 0
    s0 x y = (RUN,0,0,prog',Buffer [x,y],Buffer [])
    res s = let (HALT,_,_,_,_,Buffer ob) = snd $ runState eval $ s
            in head ob

    tracts = S.fromList $ catMaybes $ (\x y -> case res (s0 x y) of
                                                 0 -> Nothing
                                                 _ -> Just (x,y)) <$> [0..1999] <*> [0..1999]

    cands = fmap snd $ sort $ (\x y -> (x*x + y*y, (x,y))) <$> [0..1899] <*> [0..1899]

    ress = fmap fst $ filter snd $ fmap (\(x,y) -> ((x,y), fit 100 100 x y)) cands

    fit xl yl x y = (x,y) `S.member` tracts && (x+(xl-1),y) `S.member` tracts && (x,y+(yl-1)) `S.member` tracts
