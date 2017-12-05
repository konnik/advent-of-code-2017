module Puzzles.Year2017.Day5 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)
import Debug

puzzle : Puzzle
puzzle = (2017,5,"Puzzle Title", tests, part1, part2)

tests : TestSuite
tests = [ 
          (part1 "0\n3\n0\n1\n-3" == "5",  "Test part 1")
        , (part1 "" == "",  "Test part 1")
        , (part1 "" == "",  "Test part 1")
        , (part2 "" == "",  "Test part 2")
        , (part2 "" == "",  "Test part 2")
        , (part2 "0\n3\n0\n1\n-3" == "10",  "Test part 2")
        ]

part1 : PuzzleSolver
part1 input =
    stepUntilOutside { count=0, pos = 0, program = parseInput input}
    |> .count
    |> toString

part2 : PuzzleSolver
part2 input =
    stepUntilOutside2 { count=0, pos = 0, program = parseInput input}
    |> .count
    |> toString

korv input =
    stepUntilOutside { count=0, pos = 0, program = parseInput input}

type alias Program = Dict Int Int 
type alias State = {count: Int, pos: Int, program: Program }

parseInput : String -> Program
parseInput lines = 
    Dict.fromList (List.indexedMap (,) (List.map (Result.withDefault 0 << String.toInt) (String.lines lines)))

stepUntilOutside : State -> State
stepUntilOutside state = 
    let 
        newState = step state
    in
    if newState.pos <0 || newState.pos>= (Dict.size newState.program) then
        newState
    else
        stepUntilOutside newState 

step : State -> State
step state = 
    let 
        pos = state.pos
        offset = Maybe.withDefault 0 (Dict.get pos state.program)
    in
        { state | count = state.count +1
                , pos = pos + offset
                , program = (Dict.insert pos (offset+1) state.program) }
 

stepUntilOutside2 : State -> State
stepUntilOutside2 state = 
    let 
        newState = step2 state
    in
    if newState.pos <0 || newState.pos>= (Dict.size newState.program) then
        newState
    else
        stepUntilOutside2 newState 

step2 : State -> State
step2 state = 
    let 
        pos = state.pos
        offset = Maybe.withDefault 0 (Dict.get pos state.program)
    in
        { state | count = state.count + 1
                , pos = pos + offset
                , program = (Dict.insert pos (calcOffset offset) state.program) }
 
calcOffset : Int -> Int
calcOffset offset = 
    if offset >= 3 then
        offset-1
    else
        offset + 1