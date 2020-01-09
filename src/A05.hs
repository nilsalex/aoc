module A05 (a05_run,a05_input) where

import qualified Data.Sequence as S

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

op :: Int -> S.Seq Int -> Op
op pos xs
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
    x = S.index xs pos
    m1 = x `div` 10000
    m2 = (x `mod` 10000) `div` 1000
    m3 = ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    [b3, b2, b1] = fmap b [m1, m2, m3]
    p1 = S.index xs (pos+1)
    p2 = S.index xs (pos+2)
    p3 = S.index xs (pos+3)

    b i = i /= 0

runOp :: Op -> Int -> S.Seq Int -> [Int] -> IO ([Int], Either (S.Seq Int) (Int, S.Seq Int))
runOp op pos xs stack@(input,stack') =
    case op of
      Add a b c ma mb mc  -> pure (stack, Right (pos + 4, write c mc $ look a ma + look b mb))
      Mult a b c ma mb mc -> pure (stack, Right (pos + 4, write c mc $ look a ma * look b mb))
      Get a ma            -> pure (stack', Right (pos + 2, write a ma input))
      Put a ma            -> do
                              print $ look a ma
                              pure (stack, Right (pos + 2, xs))
      JumpIf a b ma mb    -> case look a ma of
                              0 -> pure (stack, Right (pos + 3, xs))
                              _ -> pure (stack, Right (look b mb, xs))
      JumpIfNot a b ma mb -> case look a ma of
                              0 -> pure (stack, Right (look b mb, xs))
                              _ -> pure (stack, Right (pos + 3, xs))
      Less a b c ma mb mc -> pure (stack, Right (pos + 4, write c mc $ case look a ma < look b mb of
                                                                         True  -> 1
                                                                         False -> 0))
      Equals a b c ma mb mc -> pure (stack, Right (pos + 4, write c mc $ case look a ma == look b mb of
                                                                           True  -> 1
                                                                         False -> 0))
      Stop                -> pure (stack, Left xs)
  where
    look i mi = case mi of
                  True -> i
                  False -> S.index xs i
    write i mi v = case mi of
                     True -> error ""
                     False -> S.update i v xs

run :: S.Seq Int -> [Int] -> IO ([Int], Int, S.Seq Int)
run xs stack = run' 0 xs stack

run' :: Int -> S.Seq Int -> IO (Int, S.Seq Int)
run' pos xs stack = do
                     (stack',r) <- runOp o pos xs
                     case r of
                       Left xs' -> return (stack', pos, xs')
                       Right (pos', xs') -> run' pos' xs' stack'
  where
    o = op pos xs

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
fileName = "data/a05/input.txt"

convProgramme :: String -> S.Seq Int
convProgramme = S.fromList . fmap read . splitAt' ','

loadProgramme :: String -> IO (S.Seq Int)
loadProgramme = fmap convProgramme . readFile

a05_input :: IO (S.Seq Int)
a05_input = loadProgramme fileName

prepare :: Int -> Int -> [Int] -> [Int]
prepare n v (x:_:_:xs) = x:n:v:xs

a05_run :: (S.Seq Int) -> IO Int
a05_run = fmap ((\xs -> S.index xs 0) . snd) . run
