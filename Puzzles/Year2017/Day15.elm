module Puzzles.Year2017.Day15 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

puzzle : Puzzle
puzzle = ( 2017, 15, "Dueling Generators", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "test-input" == "expected-output",  "Test part 1" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

part1 : PuzzleSolver
part1 input = 
    "not implemented"

part2 : PuzzleSolver
part2 input = 
    "not implemented"
