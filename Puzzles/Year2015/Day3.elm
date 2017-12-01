module Puzzles.Year2015.Day3 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Set

puzzle : Puzzle
puzzle = (2015,3,"Perfectly Spherical Houses in a Vacuum", tests, part1, part2)

tests : TestSuite
tests = [ (part1 ">" == "2",  "Part 1 - test 1")
        , (part1 "^>v<" == "4",  "Part 1 - test 2")
        , (part1 "^v^v^v^v^v" == "2",  "Part 1 - test 3")
        , (part2 "^v" == "3",  "Part 2 - test 1")
        , (part2 "^>v<" == "3",  "Part 2 - test 2")
        , (part2 "^v^v^v^v^v" == "11",  "Part 2 - test 3")
        ]


type alias Pos = (Int,  Int)


part1 : PuzzleSolver
part1 input = 
    input 
    |> String.toList 
    |> List.scanl move (0,0) 
    |> Set.fromList
    |> Set.size
    |> toString

part2 : PuzzleSolver
part2 input =
    let 
        chars = String.toList input
        santaLocations = List.scanl move (0,0) (everyOdd chars)
        roboLocations = List.scanl move (0,0) (everyEven chars)
        uniqueLocations = Set.fromList (List.append santaLocations roboLocations)
    in
        toString (Set.size uniqueLocations)
  

everyEven : List a -> List a
everyEven list = 
    case list of
        [] -> []
        a::[] -> []
        a::b::xs -> b::(everyEven xs)

everyOdd : List a -> List a
everyOdd list = 
    case list of
        [] -> []
        a::[] -> [a]
        a::b::xs -> a::(everyOdd xs)

split : List a -> (List a, List a)
split list = (everyOdd list, everyEven list)

move : Char -> Pos -> Pos
move c (x,y) = 
    case c of
        '>' -> (x+1,y)
        '<' -> (x-1,y)
        '^' -> (x,y+1)
        'v' -> (x,y-1)
        _ -> (x,y)

                