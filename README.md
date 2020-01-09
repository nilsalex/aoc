# aoc2019
Solutions for https://adventofcode.com/2019.

## Usage
    aoc2019-exe [25]

Providing `25` as argument runs the game server for day 25 on port 3000 (connect via `telnet localhost 3000`).
Otherwise, days 1 to 24 are solved.

## Building Recommendations

Building via stack is recommended, since the library depends on a fixed version of PSQueue (see `stack.yaml`). It is also recommended to set the number of threads for garbage collections to half the total number of threads, e.g. for a machine with 12 threads:

    stack build
    # for puzzles 1 to 24
    stack exec -- aoc2019-exe +RTS -N12 -qn6 -A32m -RTS
    # for the gameserver for puzzle 25
    stack exec -- aoc2019-exe +RTS -N12 -qn6 -A32m -RTS 25
