module Puzzles.Year2017.Day25 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)

puzzle : Puzzle
puzzle = ( 2017, 25, "The Halting Problem", tests, part1, part2 )

tests : TestSuite
tests = []

type Direction = Left | Right
type alias Pos = Int
type alias Value = Int
type alias Tape = Dict Pos Value 
type alias StateId = Char
type alias Blueprint = ((Int, Direction, StateId), (Int, Direction, StateId)) 
type alias Program = Dict StateId Blueprint
type alias State = { pos: Pos, tape: Tape, stateId: StateId, program : Program }

part1 : PuzzleSolver
part1 input = 
    input 
        |> parseInput
        |> initialState
        |> run 12172063
        |> .tape 
        |> checksum
        |> toString

part2 : PuzzleSolver
part2 input = 
    "not implemented"

initialState : Program -> State
initialState program = 
    { pos = 0
    , tape = Dict.empty
    , stateId = 'A'
    , program = program }

checksum : Tape -> Int
checksum tape = 
    List.sum (Dict.values tape)

run : Int -> State -> State 
run steps state = 
    if steps == 0 then
        state
    else 
        run (steps - 1) (step state)

step : State -> State
step state = 
    let
        currentValue = read state.pos state.tape
        (a,b) = blueprintForState state.stateId state.program
        (nextValue, dir, nextStateId ) = if currentValue == 0 then a else b
        nextPos = if dir == Left then state.pos - 1 else state.pos + 1
    in
        { state | pos = nextPos
                , tape = write state.pos nextValue state.tape
                , stateId = nextStateId }

blueprintForState : StateId -> Program -> Blueprint
blueprintForState id program = 
    case Dict.get id program of
        Nothing -> Debug.log ("Illegal state " ++ (toString id)) ((0,Left,'X'),(0,Left,'X'))
        Just x -> x

write : Pos -> Value ->  Tape -> Tape
write = Dict.insert

read : Pos -> Tape -> Value
read pos tape = Maybe.withDefault 0 ( Dict.get pos tape ) 


parseInput : String -> Dict StateId Blueprint
parseInput _ = 
    -- TODO: read blueprints from puzzle input 
    Dict.empty
        |> Dict.insert 'A' ( (1, Right, 'B'), (0, Left,  'C') )
        |> Dict.insert 'B' ( (1, Left,  'A'), (1, Left,  'D') )
        |> Dict.insert 'C' ( (1, Right, 'D'), (0, Right, 'C') )
        |> Dict.insert 'D' ( (0, Left,  'B'), (0, Right, 'E') )
        |> Dict.insert 'E' ( (1, Right, 'C'), (1, Left,  'F') )
        |> Dict.insert 'F' ( (1, Left,  'E'), (1, Right, 'A') )