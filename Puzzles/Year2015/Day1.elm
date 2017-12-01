module Puzzles.Year2015.Day1 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

puzzle : Puzzle
puzzle = (2015,1,"Not Quite Lisp", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "(())" == "0",  "part 1 - To floor 0")
        , (part1 "(()(()(" == "3",  "part 1 - To floor 3")
        , (part1 ")())())" == "-3",  "part 1 - To floor -3")
        , (part2 ")" == "1",  "part 2 - To floor 1")
        , (part2 "()())" == "5",  "part 2 - To floor 5")
        ]

part1 : PuzzleSolver
part1 input = 
    String.toList input
        |> List.foldl processChar 0 
        |> toString


processChar : Char -> Int -> Int
processChar ch floor = 
    case ch of
        '(' -> floor + 1
        ')' -> floor - 1
        _ -> floor

part2 : PuzzleSolver
part2 input = 
    let 
        basementFloors = 
            String.toList input
                |> List.scanl processChar 0
                |> List.indexedMap (,)
                |> List.filter (\(index,b) -> b == -1)
    in
        case List.head basementFloors of
            Nothing -> "Basement was never reached."
            Just (index, _) -> toString (index)

