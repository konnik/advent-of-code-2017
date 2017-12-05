module Puzzles.Year2017.Day5 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Array exposing (Array)

puzzle : Puzzle
puzzle = (2017,5,"A Maze of Twisty Trampolines, All Alike", tests, part1, part2)

tests : TestSuite
tests = [ 
          (part1 "0\n3\n0\n1\n-3" == "5",  "Test part 1")
        , (part2 "0\n3\n0\n1\n-3" == "10",  "Test part 2")
        ]

type alias Program = Array Int
type alias OffsetStrategy = Int-> Int
type alias State = {count: Int, pos: Int, program: Program }

part1 : PuzzleSolver
part1 input =
    startState input
        |> doUntilOutside (stepWith incrementOffset)
        |> .count
        |> toString

part2 : PuzzleSolver
part2 input =
    startState input
        |> doUntilOutside (stepWith decreaseOrIncrementOffset)
        |> .count
        |> toString

startState : String -> State
startState input = 
    { count=0, pos = 0, program = Array.fromList (List.map (Result.withDefault 0 << String.toInt) (String.lines input))}

doUntilOutside : (State -> State) -> State -> State
doUntilOutside nextState state = 
    case outside state of
        True -> state 
        False -> doUntilOutside nextState (nextState state)

outside : State -> Bool
outside state = state.pos <0 || state.pos>= (Array.length state.program)

stepWith : OffsetStrategy -> State -> State
stepWith offsetStrategy state = 
    let 
        pos = state.pos
        offset = Maybe.withDefault 0 (Array.get pos state.program)
    in
        { state | count = state.count +1
                , pos = pos + offset
                , program = (Array.set pos (offsetStrategy offset) state.program) }
 
incrementOffset : Int -> Int
incrementOffset = (+) 1

decreaseOrIncrementOffset : Int -> Int
decreaseOrIncrementOffset offset = 
    if offset >= 3 then
        offset-1
    else
        offset + 1