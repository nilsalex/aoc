module A08 (a08_input, a08_ans1, a08_ans2) where

digits :: [Char]
digits = "0123456789"

type Image a = [[[a]]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

fileName :: String
fileName = "data/a08/input.txt"

convImage :: Read a => Int -> Int -> String -> Image a
convImage r_dim c_dim xs = layers
  where
    xs' = fmap (read . pure) xs
    rows = chunksOf r_dim xs'
    layers = chunksOf c_dim rows

decode :: Image Int -> [[Int]]
decode image = fmap (fmap (\xs -> head $ dropWhile (==2) xs)) concatenated
  where
    acc0 = fmap (fmap (const ([] :: [Int]))) (head image)
    concatenated = foldr (zipWith (zipWith (:))) acc0 image

loadImage :: Read a => String -> IO (Image a)
loadImage = fmap (convImage 25 6 . filter (`elem` digits)) . readFile

a08_input :: IO (Image Int)
a08_input = loadImage fileName

a08_ans1 :: Image Int -> Int
a08_ans1 image = n1 * n2
  where
    zeroCounts = fmap (count 0 . concat) image
    layer = concat $ snd $ minimum $ zip zeroCounts image
    n1 = count 1 layer
    n2 = count 2 layer

a08_ans2 :: Image Int -> String
a08_ans2 image = unlines drawn
  where
     dec = decode image
     drawn = fmap (fmap (\x -> case x of
                                 0 -> '\x2B1B'
                                 1 -> '\x2B1C')) dec
