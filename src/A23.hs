module A23 (a23_input,a23_ans1,a23_ans2) where

import Debug.Trace (trace)

import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Foldable (toList,foldl')
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import qualified Data.Set as SET

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

eval :: State ProgState (Code, Maybe (Int,Int,Int))
eval =
  do
     (code, ip, bp, prog, istack, ostack) <- get
     case code of
       HALT -> return (HALT, Nothing)
       _    -> 
         let o = op ip prog in
         case o of
           Stop ->
             put (HALT, ip, bp, prog, istack, ostack) >> return (HALT, Nothing)
           Put a ma ->
             case fromBuffer ostack of
               x:c:os -> put (OUT, ip + 2, bp, prog, istack, Buffer os) >> return (OUT, Just (c,x,look a ma bp prog))
               _        -> put (RUN, ip + 2, bp, prog, istack, Buffer $ look a ma bp prog : fromBuffer ostack) >> eval
           Get a ma ->
             case fromBuffer istack of
               [] -> put (WAIT, ip, bp, prog, istack, ostack) >> return (WAIT, Nothing)
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
                                        REL -> S.update (b+i) v xs
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
fileName = "data/a23/input.txt"

convProgramme :: String -> Programme
convProgramme = Programme . S.fromList . fmap read . splitAt' ','

loadProgramme :: String -> IO Programme
loadProgramme = fmap convProgramme . readFile

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

a23_input :: IO Programme
a23_input = loadProgramme fileName

a23_ans1 :: Programme -> Int
a23_ans1 (Programme prog') = go comps
  where
    prog = Programme $ prog' S.>< S.replicate 10000 0
    comp x = (RUN,0,0,prog,Buffer [x,-1],Buffer [])
    comps = fmap comp $ S.fromList [0..49]

    go _comps = case M.lookup 255 packets of
                     Just xs -> last xs
                     Nothing -> go comps''
      where
        comps' = fmap (runState eval) _comps
        packets = foldl' (\acc ((_,out),_) -> case out of
                                                Nothing -> acc
                                                Just (c,x,y) -> M.insertWith (++) c [x,y] acc) M.empty comps'
        comps'' = S.mapWithIndex (\k (_,(x1,x2,x3,x4,Buffer ib,ob)) -> let ib' = case M.lookup k packets of
                                                                                   Nothing -> ib
                                                                                   Just xs -> ib ++ xs
                                                                       in  (x1,x2,x3,x4,Buffer ib',ob)) comps'

a23_ans2 :: Programme -> Int
a23_ans2 (Programme prog') = go comps (0,[0,0])
  where
    prog = Programme $ prog' S.>< S.replicate 10000 0
    comp x = (RUN,0,0,prog,Buffer [x,-1],Buffer [])
    comps = fmap comp $ S.fromList [0..49]

    go _comps (prev,nat) =
          if M.null packets && all (\((c,_),_) -> case c of
                                                    WAIT -> True
                                                    _    -> False) comps'
          then let comps''' = S.adjust (\(x1,x2,x3,x4,Buffer ib,x5) -> (x1,x2,x3,x4,Buffer (ib ++ nat),x5)) 0 comps''
               in if last nat == prev
                  then prev
                  else go comps''' (last nat,nat)
          else case M.lookup 255 packets of
                 Just [x,y] -> let p' = [x,y]
                               in go comps'' (prev,p')
                 Nothing -> go comps'' (prev,nat)
      where
        comps' = fmap (runState eval) _comps
        packets = foldl' (\acc ((_,out),_) -> case out of
                                                Nothing -> acc
                                                Just (c,x,y) -> M.insertWith (++) c [x,y] acc) M.empty comps'
        comps'' = S.mapWithIndex (\k (_,(x1,x2,x3,x4,Buffer ib,ob)) -> let ib' = case M.lookup k packets of
                                                                                   Nothing -> case ib of
                                                                                                [] -> [-1]
                                                                                                _  -> ib
                                                                                   Just xs -> ib ++ xs
                                                                       in  (x1,x2,x3,x4,Buffer ib',ob)) comps'
