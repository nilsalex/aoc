module Lib
    ( someFunc
    ) where

import A01
import A02

someFunc :: IO ()
someFunc = do
            a01_i <- a01_input
            putStrLn "Day 1:"
            putStrLn $ "Answer 1 : " ++ (show $ a01_ans1 a01_i)
            putStrLn $ "Answer 2 : " ++ (show $ a01_ans2 a01_i)

            a02_i <- a02_input
            putStrLn ""
            putStrLn "Day 2:"
            putStrLn $ "Answer 1 : " ++ (show $ a02_ans1 a02_i)
            putStrLn $ "Answer 2 : " ++ (show $ a02_ans2 a02_i)

