module A05 (a05_run,a05_input) where

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

runOp :: Op -> Int -> [Int] -> IO (Either [Int] (Int, [Int]))
runOp op pos xs =
    case op of
      Add a b c ma mb mc  -> pure $ Right $ (pos + 4, write c mc $ look a ma + look b mb)
      Mult a b c ma mb mc -> pure $ Right $ (pos + 4, write c mc $ look a ma * look b mb)
      Get a ma            -> do
                              s <- getLine
                              pure $ Right $ (pos + 2, write a ma (read s :: Int))
      Put a ma            -> do
                              print $ look a ma
                              pure $ Right (pos + 2, xs)
      JumpIf a b ma mb    -> case look a ma of
                              0 -> pure $ Right (pos + 3, xs)
                              _ -> pure $ Right (look b mb, xs)
      JumpIfNot a b ma mb -> case look a ma of
                              0 -> pure $ Right (look b mb, xs)
                              _ -> pure $ Right (pos + 3, xs)
      Less a b c ma mb mc -> pure $ Right $ (pos + 4, write c mc $ case look a ma < look b mb of
                                                                     True  -> 1
                                                                     False -> 0)
      Equals a b c ma mb mc -> pure $ Right $ (pos + 4, write c mc $ case look a ma == look b mb of
                                                                       True  -> 1
                                                                       False -> 0)
      Stop                -> pure $ Left xs
  where
    look i mi = case mi of
                  True -> i
                  False -> xs !! i
    write i mi v = case mi of
                     True -> error ""
                     False -> modify i (const v) xs

run :: [Int] -> IO (Int, [Int])
run = run' 0

run' :: Int -> [Int] -> IO (Int, [Int])
run' pos xs = do
               r <- runOp o pos xs
               case r of
                 Left xs' -> return (pos, xs')
                 Right (pos', xs') -> run' pos' xs'
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

convProgramme :: String -> [Int]
convProgramme = fmap read . splitAt' ','

loadProgramme :: String -> IO [Int]
loadProgramme = fmap convProgramme . readFile

a05_input :: IO [Int]
a05_input = loadProgramme fileName

prepare :: Int -> Int -> [Int] -> [Int]
prepare n v (x:_:_:xs) = x:n:v:xs

a05_run :: [Int] -> IO Int
a05_run = fmap (head . snd) . run
