module A13 (a13_input,a13_ans1,a13_ans2) where

import qualified Data.Sequence as S
import qualified Data.Set as SET
import Control.Monad.Trans
import Control.Monad.Trans.State
                                
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

appendIBuf :: Int -> State ProgState ()
appendIBuf i = modify (\(x1,x2,x3,x4,Buffer is,x5) -> (x1,x2,x3,x4,Buffer (i:is),x5))

readOBuf :: State ProgState Buffer
readOBuf = do
            (x1,x2,x3,x4,x5,os) <- get
            put (x1,x2,x3,x4,x5,Buffer [])
            return os

eval :: State ProgState Code
eval =
  do
     (code, ip, bp, prog, istack, ostack) <- get
     case code of
       HALT -> return HALT
       _    -> 
         let o = op ip prog in
         case o of
           Stop ->
             put (HALT, ip, bp, prog, istack, ostack) >> return HALT
           Put a ma ->
             case fromBuffer ostack of
               _:_:[] -> put (OUT, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> return OUT
               _      -> put (RUN, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> eval
           Get a ma ->
             case fromBuffer istack of
               [] -> put (WAIT, ip, bp, prog, istack, ostack) >> return WAIT
               (i:is) -> put (RUN, ip + 2, bp, write a ma bp i prog, Buffer is, ostack) >> eval
           Add a b c ma mb mc ->
             put (RUN, ip + 4, bp, write c mc bp (look a ma bp prog + look b mb bp prog) prog, istack, ostack) >> eval
           Mult a b c ma mb mc ->
             put (RUN, ip + 4, bp, write c mc bp (look a ma bp prog * look b mb bp prog) prog, istack, ostack) >> eval
           Less a b c ma mb mc ->
             put (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog < look b mb bp prog) prog, istack, ostack) >> eval
           Equals a b c ma mb mc ->
             put (RUN, ip + 4, bp, write c mc bp (i $ look a ma bp prog == look b mb bp prog) prog, istack, ostack) >> eval
           JumpIf a b ma mb ->
             case toB $ look a ma bp prog of
               True -> put (RUN, look b mb bp prog, bp, prog, istack, ostack) >> eval
               False -> put (RUN, ip + 3, bp, prog, istack, ostack) >> eval
           JumpIfNot a b ma mb ->
             case toB $ look a ma bp prog of
               False -> put (RUN, look b mb bp prog, bp, prog, istack, ostack) >> eval
               True -> put (RUN, ip + 3, bp, prog, istack, ostack) >> eval
           Base a ma ->
             put (RUN, ip + 2, bp + look a ma bp prog, prog, istack, ostack) >> eval

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
fileName = "data/a13/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . (S.>< S.replicate 10000 0) . S.fromList . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

a13_input :: IO Programme
a13_input = loadProgramme fileName

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

type Pos = (Int,Int)
type GameState = (SET.Set Pos, Pos, Pos, Int) -- block set,
                                              -- ball pos,
                                              -- paddle pos,
                                              -- score

waiting :: State ProgState Bool
waiting = do
           (code,_,_,_,_,_) <- get
           case code of
             WAIT -> return True
             _    -> return False

setRun :: State ProgState ()
setRun = modify (\(_,x1,x2,x3,x4,x5) -> (RUN,x1,x2,x3,x4,x5))

runGame :: StateT GameState (State ProgState) Int
runGame = do
           (curBlocks, curBall, curPad, curScore) <- get
           w <- lift $ waiting
           if w
           then do
                 lift $ appendIBuf $ signum $ fst curBall - fst curPad
                 lift $ setRun
                 runGame
           else do
                 lift eval
                 Buffer ob <- lift readOBuf
                 case ob of
                   [t,y,x] -> case (x,y) of
                                (-1,0) -> modify (\(bs,b,p,_) -> (bs,b,p,t)) >> runGame
                                _      -> case t of
                                            0 -> modify (\(bs,b,p,s) -> (SET.delete (x,y) bs,b,p,s)) >> runGame
                                            2 -> modify (\(bs,b,p,s) -> (SET.insert (x,y) bs,b,p,s)) >> runGame
                                            3 -> modify (\(bs,b,_,s) -> (bs,b,(x,y),s)) >> runGame
                                            4 -> modify (\(bs,_,p,s) -> (bs,(x,y),p,s)) >> runGame
                                            _ -> runGame
                   _       -> do
                               (code,_,_,_,_,_) <- lift $ get
                               case code of
                                 HALT -> return curScore
                                 _    -> runGame

a13_ans1 :: Programme -> Int
a13_ans1 prog = SET.size blocks
  where
    s0 = (RUN, 0, 0, prog, Buffer [], Buffer [])
    game = (SET.empty, (0,0), (0,0), 0)
    (blocks,_,_,_) = evalState (execStateT runGame $ game) $ s0

a13_ans2 :: Programme -> Int
a13_ans2 (Programme (x S.:<| prog)) = evalState (evalStateT runGame $ game) $ s0
  where
    s0 = (RUN, 0, 0, Programme (2 S.:<| prog), Buffer [], Buffer [])
    game = (SET.empty, (0,0), (0,0), 0)
