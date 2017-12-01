module Puzzles.Year2017.DayX exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

{- 
Template for solving an Advent of Code puzzle

Don't forget to add this puzzle to allPuzzles list in App.elm

Good luck!!
-}

puzzle : Puzzle
puzzle = (2017,999,"Puzzle Title", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "test-input" == "expected-output",  "Test part 1")
        , (part2 "test-input" == "expected-output",  "Test part 2")
        ]

part1 : PuzzleSolver
part1 input = "not implemented"

part2 : PuzzleSolver
part2 input = "not implemented"
