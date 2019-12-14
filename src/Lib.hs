module Lib
    ( someFunc
    ) where

import A01

someFunc :: IO ()
someFunc = do
            a01_i <- a01_input
            putStrLn "Day 1:"
            putStrLn $ "Answer 1 : " ++ (show $ a01_ans1 a01_i)
            putStrLn $ "Answer 2 : " ++ (show $ a01_ans2 a01_i)

            putStrLn ""
            putStrLn "Day 2:"
