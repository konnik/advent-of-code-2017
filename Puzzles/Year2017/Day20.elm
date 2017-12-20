module Puzzles.Year2017.Day20 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)


puzzle : Puzzle
puzzle = ( 2017, 20, "Particle Swarm", tests, part1, part2 )

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


dist : (Int, Int, Int) -> Int
dist (x,y,z) = abs x + abs y + abs z

parseInput : String -> List (Int, Int, Int)
parseInput input = 
    String.words input 
    |> List.map parseLine

parseLine : String -> (Int, Int, Int)
parseLine line = 
    let
        a = case String.split "a=" line of
            [_,x] -> x
            _ -> "<0,0,0,>"
        
        b = String.fromList (List.filter (\ c -> c /= '<' && c /= '>') (String.toList a))
        (x,y,z) = case String.split "," b of
                    [x,y,z] -> (toInt x, toInt y , toInt z) 
                    _ -> (0,0,0)
    in
        (x,y,z)
        
toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt


--  