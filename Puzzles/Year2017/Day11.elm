module Puzzles.Year2017.Day11 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)


puzzle : Puzzle
puzzle = ( 2017, 11, "Hex Ed", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "ne,ne,ne" == "3",  "Test part 1" )
    , ( part1 "ne,ne,sw,sw" == "0",  "Test part 1" )
    , ( part1 "ne,ne,s,s" == "2",  "Test part 1" )
    , ( part1 "se,sw,se,sw,sw" == "3",  "Test part 1" )
    , ( part2 "ne,ne,sw,sw" == "2",  "Test part 2" )
    ]

type alias Pos = (Int, Int)
type alias State = { pos: Pos, dists: List Int}

part1 : PuzzleSolver
part1 input = 
    parseInput input 
        |> List.foldl step {pos = (0,0), dists = []} 
        |> .pos
        |> dist
        |> toString
    
part2 : PuzzleSolver
part2 input = 
    parseInput input 
        |> List.foldl step {pos = (0,0), dists = []} 
        |> .dists
        |> List.maximum
        |> Maybe.withDefault 0 
        |> toString

dist : Pos -> Int
dist (x,y) = 
    max (abs x) (abs y)

step : String -> State -> State
step dir state= 
    let
        (x,y) = state.pos
        newPos = 
            case dir of 
                "n" ->  (x + 0, y + 1)
                "s" ->  (x + 0, y - 1)
                "sw" -> (x - 1, y - 1)
                "se" -> (x + 1, y + 0)
                "ne" -> (x + 1, y + 1)
                "nw" -> (x - 1, y + 0)
                _ -> state.pos
        newDists = (dist newPos) :: state.dists
    in
        { pos = newPos, dists = newDists }


parseInput : String -> List String
parseInput = String.split ","