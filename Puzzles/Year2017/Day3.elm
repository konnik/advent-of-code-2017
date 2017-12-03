module Puzzles.Year2017.Day3 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)

puzzle : Puzzle
puzzle = (2017,3,"Spiral Memory", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "1" == "0",  "Part 1 - square 1")
        , (part1 "2" == "1",  "Part 1 - square 2")
        , (part1 "12" == "3",  "Part 1 - square 12")
        , (part1 "23" == "2",  "Part 1 - square 23")
        , (part1 "1024" == "31",  "Part 1 - square 1024")
        , (part2 "4" == "5",  "Part 2 - larger than 4")
        , (part2 "5" == "10",  "Part 2 - larger than 5")
        ]

type alias Pos = (Int, Int)
type alias Dir = (Int, Int)
type alias Values = Dict Pos Int
type alias Step = 
            { n: Int
            , pos: Pos
            , dir: Dir
            , nextTurn: Int
            , step1: Int 
            , step2: Int
            , value : Int
            , values : Values
            }

start : Step
start = { n = 1, pos = (0,0), dir = (1,0), nextTurn = 2, step1 = 1, step2 = 2, value = 1, values = Dict.singleton (0,0) 1}

part1 : PuzzleSolver
part1 input = 
    start
        |> stepUntil (squareIs (parseInput input)) [step, turn]
        |> .pos
        |> manhattanDistance
        |> toString

part2 : PuzzleSolver
part2 input = 
    start
        |> stepUntil (squareValueLargerThan (parseInput input)) [step, turn, calcValue]
        |> .value
        |> toString

parseInput : String -> Int
parseInput = Result.withDefault 0 << String.toInt

stepUntil : (Step -> Bool) -> List (Step -> Step) -> Step -> Step
stepUntil done functions step = 
    case done step of
        True -> step 
        False -> stepUntil done functions (chain functions step)

squareIs : Int-> Step -> Bool
squareIs value step = step.n == value

squareValueLargerThan : Int -> Step -> Bool
squareValueLargerThan value step = step.value > value 

manhattanDistance : Pos -> Int
manhattanDistance (x, y) = (abs x) + (abs y)

step : Step -> Step 
step step = 
    { step | n = step.n + 1, pos = nextPos step.dir step.pos }

turn : Step -> Step
turn step =
    if step.n == step.nextTurn then
        { step | dir = nextDir step.dir
               , nextTurn = step.nextTurn + step.step1
               , step1 = step.step2
               , step2 = step.step1 + 1 }
    else
        step

chain : List (a -> a) -> a -> a
chain functions a = 
    case functions of
        [] -> a
        f::fs -> chain fs (f a)  

nextPos : Dir -> Pos -> Pos
nextPos (dx, dy) (x, y) = (x+dx, y+dy)

nextDir : Dir -> Dir
nextDir (dx, dy) = (-dy, dx)

addIfPresent : Maybe Int -> Int -> Int
addIfPresent maybeA b = 
    case maybeA of 
        Nothing -> b
        Just a -> a + b

calcValue : Step -> Step
calcValue step = 
    let 
        (x,y) = step.pos
        values = step.values
        value = 0 
            |> addIfPresent (Dict.get (x + 1, y + 0) values)
            |> addIfPresent (Dict.get (x + 1, y + 1) values)
            |> addIfPresent (Dict.get (x + 0, y + 1) values)
            |> addIfPresent (Dict.get (x - 1, y + 1) values)
            |> addIfPresent (Dict.get (x - 1, y + 0) values)
            |> addIfPresent (Dict.get (x - 1, y - 1) values)
            |> addIfPresent (Dict.get (x + 0, y - 1) values)
            |> addIfPresent (Dict.get (x + 1, y - 1) values)
    in
        {step | value = value, values = Dict.insert (x,y) value values}
