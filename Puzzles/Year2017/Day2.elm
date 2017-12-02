module Puzzles.Year2017.Day2 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)


puzzle : Puzzle
puzzle = (2017,2,"Corruption Checksum", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "5 1 9 5\n7 5 3\n2 4 6 8" == "18",  "Test part 1")
        , (part2 "5 9 2 8\n9 4 7 3\n3 8 6 5" == "9",  "Test part 2")
        ]

part1 : PuzzleSolver
part1 = checksum minMaxDifference

part2 : PuzzleSolver
part2 = checksum division


checksum : (List Int -> Int) -> String -> String
checksum algorithm input = 
    (
    String.lines input
        |> List.map parseLine
        |> List.map algorithm
    ) |> List.sum |> toString


parseLine : String -> List Int
parseLine input = 
    String.words input
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)


minMaxDifference : List Int -> Int
minMaxDifference list = 
    let 
        min = (Maybe.withDefault 0 << List.minimum) list 
        max = (Maybe.withDefault 0 << List.maximum) list 
    in
        max - min 

divadable : Int -> Int -> Bool
divadable a b =
    rem a b == 0

findDivisor : Int -> List Int -> Maybe Int
findDivisor a list = 
    case List.filter (divadable a) list of
        [] -> Nothing
        a::xs -> Just a

division : List Int -> Int
division list = 
    division2 ((List.reverse << List.sort) list)

division2 : List Int -> Int
division2 list = 
    case list of
        [] -> 0
        [_] -> 0
        a::rest -> 
            case findDivisor a rest of 
                Nothing -> division2 rest
                Just b -> a // b 