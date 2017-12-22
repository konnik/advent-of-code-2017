module Puzzles.Year2017.Day22 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Set exposing (Set)

puzzle : Puzzle
puzzle = ( 2017, 22, "Sporifica Virus", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( solve 10000 (1,1) "..#\n#..\n..." == 5587,  "Test part 1" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type Node 
    = Clean
    | Weakened
    | Infected
    | Flagged 

type alias Pos = (Int, Int)
type alias Dir = (Int, Int)
type alias Infected = Set Pos
type alias State = { pos: Pos, dir : Dir, infected: Infected, infectionCount: Int}

part1 : PuzzleSolver
part1 input = 
    solve 10000 (12,12) input
        |> toString

part2 : PuzzleSolver
part2 input = 
    "not implemented"

solve : Int -> Pos -> String -> Int
solve bursts startPos input = 
    parseInput input
        |> initialState startPos
        |> repeat bursts step
        |> .infectionCount

initialState : Pos -> Infected -> State
initialState pos infected = 
    { pos = pos, dir = (0,-1), infected = infected, infectionCount = 0}

countInfected : State -> Int
countInfected state = 
    Set.size state.infected

repeat : Int -> (State -> State) -> State -> State
repeat n step state = 
    if n == 0 then
        state
    else 
        repeat (n-1) step (step state) 

turn : State -> State
turn state = 
    let 
        (dx, dy) = state.dir
    in
        if Set.member state.pos state.infected then
            { state | dir = (-dy, dx) }
        else
            { state | dir = (dy, -dx) }

move : State -> State
move state = 
    let 
        (x, y) = state.pos
        (dx, dy) = state.dir
    in
            { state | pos = (x + dx , y + dy) }

infect : State -> State
infect state = 
    if Set.member state.pos state.infected then
        { state | infected = Set.remove state.pos state.infected }
    else
        { state | infected = Set.insert state.pos state.infected, infectionCount = state.infectionCount + 1 }


step : State -> State
step state =
    state
        |> turn
        |> infect
        |> move

parseInput : String -> Infected
parseInput input = 
    String.lines input
        |> List.indexedMap (,)
        |> List.map parseLine
        |> List.concat
        |> Set.fromList

parseLine : (Int, String) -> List Pos
parseLine (y, line) = 
    String.toList line 
        |> List.indexedMap (\ x ch -> (x,y,ch))
        |> List.filter (\ (_,_,ch) -> ch == '#')
        |> List.map (\(x,y,_) -> (x,y))

