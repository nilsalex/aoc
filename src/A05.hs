module A05 (a05_run1,a05_input) where

data Op = Add Int Int Int Bool Bool Bool |
          Mult Int Int Int Bool Bool Bool |
          Get Int Bool |
          Put Int Bool |
          Stop

add :: Int
add = 1

mult :: Int
mult = 2

get :: Int
get = 3

put :: Int
put = 4

stop :: Int
stop = 99

op :: [Int] -> Op
op (x:xs)
    | o == add = Add p1 p2 p3 b1 b2 b3
    | o == mult = Mult p1 p2 p3 b1 b2 b3
    | o == get = Get p1 b1
    | o == put = Put p1 b1
    | o == stop = Stop
  where
    m1 = x `div` 10000
    m2 = (x `mod` 10000) `div` 1000
    m3 = ((x `mod` 10000) `mod` 1000) `div` 100
    o  = (((x `mod` 10000) `mod` 1000) `mod` 100)

    [b3, b2, b1] = fmap b [m1, m2, m3]
    [p1, p2, p3] = take 3 xs

    b i = i /= 0

runOp :: Op -> [Int] -> IO (Either [Int] (Int, [Int]))
runOp op xs =
    case op of
      Add a b c ma mb mc  -> pure $ Right $ (4, write c mc $ look a ma + look b mb)
      Mult a b c ma mb mc -> pure $ Right $ (4, write c mc $ look a ma * look b mb)
      Get a ma            -> do
                              s <- getLine
                              pure $ Right $ (2, write a ma (read s :: Int))
      Put a ma            -> do
                              print $ look a ma
                              pure $ Right (2, xs)
      Stop                -> pure $ Left xs
  where
    look i mi = case mi of
                  True -> i
                  False -> xs !! i
    write i mi v = case mi of
                     True -> error ""
                     False -> modify i (const v) xs

run :: [Int] -> IO [Int]
run = run' 0

run' :: Int -> [Int] -> IO [Int]
run' pos xs = do
               r <- runOp o xs
               case r of
                 Left xs' -> return xs'
                 Right (s, xs') -> run' (pos + s) xs'
  where
    o = op $ drop pos xs

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

a05_run1 :: [Int] -> IO Int
a05_run1 = fmap head . run
