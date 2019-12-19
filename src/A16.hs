module A16 (a16_input) where

fileName :: String
fileName = "data/a16/input.txt"

parseList :: String -> [Int]
parseList = fmap (read . pure) . head . lines

a16_input :: IO [Int]
a16_input = fmap parseList $ readFile fileName
