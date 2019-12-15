module Lib
    ( someFunc
    ) where

import System.IO (hFlush, stdout)

import A01
import A02
import A03
import A04
import A05
import A06

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

            a03_i <- a03_input
            putStrLn ""
            putStrLn "Day 3:"
            putStrLn $ "Answer 1 : " ++ (show $ a03_ans1 a03_i)
            putStrLn $ "Answer 2 : " ++ (show $ a03_ans2 a03_i)

            putStrLn ""
            putStrLn "Day 4:"
            putStrLn $ "Answer 1 : " ++ (show a04_ans1)
            putStrLn $ "Answer 2 : " ++ (show a04_ans2)

            a05_i <- a05_input
            putStrLn ""
            putStrLn "Day 5:"
            putStrLn $ "Run 1 : "
            putStr "Enter 1 : "
            hFlush stdout
            res <- a05_run a05_i
            
            putStrLn ""
            putStrLn $ "Run 2 : "
            putStr "Enter 5 : "
            hFlush stdout
            res <- a05_run a05_i

            a06_i <- a06_input
            putStrLn ""
            putStrLn "Day 6:"
            putStrLn $ "Answer 1 : " ++ (show $ a06_ans1 a06_i)
            
            return ()
