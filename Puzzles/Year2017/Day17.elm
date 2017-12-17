module Puzzles.Year2017.Day17 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE exposing (elemIndex, getAt)



puzzle : Puzzle
puzzle = ( 2017, 17, "Spinlock", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "3" == "638",  "Test part 1" )
    --, ( part2 "3" == "9",  "Test part 2" )
    ]

type alias State = { count: Int, pos: Int, buffer: List Int, valueAfterZero: Int}

part1 : PuzzleSolver
part1 input = 
    stepUntil (countIs 2017) (step (toInt input)) initialState
        |> valueAfter 2017
        |> toString

part2 : PuzzleSolver
part2 input = 
    stepUntil (countIs 50000000) (stepAndTrackValueAfterZero (toInt input)) initialState
        |> .valueAfterZero 
        |> toString

initialState : State 
initialState = { count = 0, pos = 0, buffer = [0], valueAfterZero = 0 }

valueAfter : Int -> State -> Int
valueAfter value state =
    case elemIndex value state.buffer of
        Just index -> 
            getAt ((index + 1) % (List.length state.buffer)) state.buffer
                |> Maybe.withDefault 0 
        _ -> 0 

countIs : Int -> State -> Bool
countIs n state = n == state.count  

stepUntil : (State -> Bool) -> (State -> State) -> State -> State 
stepUntil done stepFunc state = 
    if done state then
        state
    else 
        stepUntil done stepFunc (stepFunc state)

step : Int -> State -> State 
step n state = 
    let
        newPos = ((state.pos + n) % (List.length state.buffer)) + 1
        newCount = state.count + 1 
        newBuffer = insertAfter (newPos-1) newCount state.buffer
    in
        { state | pos = newPos , buffer = newBuffer, count = newCount}

stepAndTrackValueAfterZero : Int -> State -> State
stepAndTrackValueAfterZero n state = 
    let
        newPos = ((state.pos + n) % (state.count +1)) + 1
        newCount = state.count + 1 
        newValueAfterZero = if newPos == 1 then newCount else state.valueAfterZero
    in
        { state | pos = newPos, count = newCount, valueAfterZero = newValueAfterZero}


insertAfter : Int -> Int -> List Int -> List Int
insertAfter pos value buffer = 
    let 
        head = List.take (pos+1) buffer
        tail = List.drop (pos+1) buffer
    in
        head ++ [value] ++ tail 

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

