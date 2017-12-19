module Puzzles.Year2017.Day19 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict) 
import Tuple
import Char

puzzle : Puzzle
puzzle = ( 2017,19, "A Series of Tubes", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ " == "ABCDEF",  "Test part 1" )
    , ( part2 "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ " == "38",  "Test part 2" )
    ]

type alias Map =  Dict (Int, Int)  Char
type alias Pos = (Int, Int)
type alias Dir = (Int, Int)
type alias State = { pos : Pos, dir: Dir, letters: List Char, finished: Bool, map: Map, steps: Int }

initialState : Map -> State 
initialState map = 
    { pos  = startPos map
    , dir = (0, 1)
    , letters = []
    , map = map
    , finished = False
    , steps = 1
    }

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> initialState
        |> stepUntilFinish
        |> .letters
        |> String.fromList

part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> initialState
        |> stepUntilFinish
        |> .steps
        |> toString


stepUntilFinish : State -> State
stepUntilFinish state = 
    if state.finished then 
        state
    else
        stepUntilFinish (step state)


step : State -> State
step state = 
    state 
        |> move
        |> turn
        |> pickLetter

pickLetter : State -> State
pickLetter state = 
    case Dict.get state.pos state.map of
        Nothing -> state
        Just ch -> 
            if Char.isUpper ch then
                { state | letters = state.letters ++ [ch] }
            else
                state

turn : State -> State
turn state = 
    let
        (x,y) = state.pos
        (dx,dy) = state.dir
        forwardChar = Dict.get (x + dx, y + dy) state.map
        leftChar = Dict.get (x + dy, y - dx) state.map
        rightChar = Dict.get (x - dy, y + dx) state.map
    in
        if forwardChar /= Nothing then 
            state
        else if leftChar /= Nothing then
            { state | dir = (dy, -dx) }
        else if rightChar /= Nothing then
            { state | dir = (-dy, dx) }
        else
            { state | finished = True }


move : State -> State
move state = 
    let
        (x,y) = state.pos
        (dx,dy) = state.dir
    in
        { state | pos = ( x + dx, y + dy ), steps = state.steps + 1 }


startPos : Map -> Pos
startPos map = 
    Dict.toList map 
        |> List.filter (\ ((x,y), ch) -> y == 0 && ch == '|')
        |> List.head
        |> Maybe.withDefault ((0,0),'|')
        |> Tuple.first


parseInput : String -> Dict Pos  Char
parseInput input = 
    String.lines input 
        |> List.indexedMap (\ y line -> parseLine y line )
        |> List.concat
        |> List.filter (\ (_,ch) -> ch /= ' ')
        |> Dict.fromList 

parseLine : Int -> String -> List (Pos, Char)
parseLine y line = 
    List.indexedMap (\ x ch -> ((x,y), ch) ) (String.toList line) 
