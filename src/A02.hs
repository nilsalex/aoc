module A02 (a02_input, a02_ans1, a02_ans2) where

stop :: Int
stop = 99

add :: Int
add = 1

mult :: Int
mult = 2

run :: [Int] -> [Int]
run = run'' 0

run'' :: Int -> [Int] -> [Int]
run'' pos xs
    | opcode == stop = xs
    | opcode == add  = run'' pos' $ modify ires (const s) xs
    | opcode == mult = run'' pos' $ modify ires (const m) xs
    | otherwise = error $ show (pos, xs)
  where
    opcode = xs !! pos

    [_,ia,ib,ires] = take 4 $ drop pos xs
    
    a = xs !! ia
    b = xs !! ib

    s = a + b
    m = a * b

    pos' = pos + 4

run' :: ([Int], [Int]) -> ([Int], [Int])
run' (xs, ys@(y:ys'))
    | y == stop = (xs, ys)
    | y == add = let (xs_new, ys_new) = update ires s xs ys
                 in run' (xs_new ++ take 4 ys_new, drop 4 ys_new)
    | y == mult = let (xs_new, ys_new) = update ires m xs ys
                  in run' (xs_new ++ take 4 ys_new, drop 4 ys_new)
    | otherwise = error $ show xs ++ " ## " ++ show y ++ " ## " ++ show ys'
  where
    [ia, ib, ires] = take 3 ys'

    a = (xs ++ ys) !! ia
    b = (xs ++ ys) !! ib

    s = a + b
    m = a * b

    update _i _r _xs _ys = 
      if _i < length _xs
      then (modify _i (const _r) _xs, _ys)
      else (_xs, modify _i (const _r) _ys)

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
fileName = "data/a02/input.txt"

convProgramme :: String -> [Int]
convProgramme = fmap read . splitAt' ','

loadProgramme :: String -> IO [Int]
loadProgramme = fmap convProgramme . readFile

a02_input :: IO [Int]
a02_input = loadProgramme fileName

prepare :: Int -> Int -> [Int] -> [Int]
prepare n v (x:_:_:xs) = x:n:v:xs

a02_ans1 :: [Int] -> Int
a02_ans1 = head . run . prepare 12 2

a02_ans2 :: [Int] -> Int
a02_ans2 prog = 100 * n + v
  where
    (_, (n, v)) = head $
                  filter fst $
                  (\a b -> (19690720 == (head . run . prepare a b) prog, (a,b))) <$> [0..99] <*> [0..99]
