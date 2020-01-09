module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
          []      -> a01_to_a24
          "25":[] -> a25
          _       -> putStrLn "Usage: aoc2019-exe [25]\n\n\
                               \Solves the puzzles from https://adventofcode.com/2019\n\
                               \Providing 25 as argument runs the game server for day 25 on port 3000. (connect via telnet localhost 3000)\n\
                               \Otherwise, days 1 to 24 are solved."
