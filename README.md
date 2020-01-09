# aoc
Solutions for https://adventofcode.com/2019

Usage: `aoc-exe [25]`

Solves the puzzles from https://adventofcode.com/2019
Providing `25` as argument runs the game server for day 25 on port 3000 (connect via `telnet localhost 3000`).
Otherwise, days 1 to 24 are solved.

## Building Recommendations

Building via stack is recommended, since the library depends on a fixed version of PSQueue (see `stack.yaml`):

    stack build
    stack exec -- aoc-exe +RTS -N [number of cores] -qn [N `div` 2] -A32m -RTS
