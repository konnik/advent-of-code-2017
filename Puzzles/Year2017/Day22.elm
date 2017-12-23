module Puzzles.Year2017.Day22 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Dict exposing (Dict)

puzzle : Puzzle
puzzle = ( 2017, 22, "Sporifica Virus", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( solve1 10000 (1,1) "..#\n#..\n..." == 5587,  "Test part 1" )
    ]

type Node 
    = Clean
    | Weakened
    | Infected
    | Flagged 

type alias Pos = (Int, Int)
type alias Dir = (Int, Int)
type alias Infected = Dict Pos Node
type alias State = { pos: Pos, dir : Dir, infected: Infected, infectionCount: Int}

part1 : PuzzleSolver
part1 input = 
    solve1 10000 (12,12) input
        |> toString

part2 : PuzzleSolver
part2 input = 
    solve2 10000000 (12,12) input
        |> toString

initialState : Pos -> Infected -> State
initialState pos infected = 
    { pos = pos, dir = (0,-1), infected = infected, infectionCount = 0}

-- solution for part 1

solve1 : Int -> Pos -> String -> Int
solve1 bursts startPos input = 
    parseInput input
        |> initialState startPos
        |> repeat bursts step1
        |> .infectionCount

turn1 : State -> State
turn1 state = 
    let 
        (dx, dy) = state.dir
    in  
        case Maybe.withDefault Clean (Dict.get state.pos state.infected) of
            Clean -> { state | dir = (dy, -dx) }
            Infected -> { state | dir = (-dy, dx) }
            _ -> Debug.log "turn1: Invalid state!" state

infect1 : State -> State
infect1 state = 
    let 
        newNodeState = 
            case Maybe.withDefault Clean (Dict.get state.pos state.infected) of
                Clean -> Infected
                Infected -> Clean 
                _ -> Debug.log "Infect1: illegal state!" Clean
        
        newInfectionCount = 
            if newNodeState == Infected then 
                state.infectionCount + 1
            else 
                state.infectionCount
    in
        { state | infected = Dict.insert state.pos newNodeState state.infected, infectionCount = newInfectionCount}

step1 : State -> State
step1 state =
    state
        |> turn1
        |> infect1
        |> move

-- solution for part 2

solve2 : Int -> Pos -> String -> Int
solve2 bursts startPos input = 
    parseInput input
        |> initialState startPos
        |> repeat bursts step2
        |> .infectionCount



turn2 : State -> State
turn2 state = 
    let 
        (dx, dy) = state.dir
    in  
        case Maybe.withDefault Clean (Dict.get state.pos state.infected) of
            Clean -> { state | dir = (dy, -dx) }
            Weakened -> state
            Infected -> { state | dir = (-dy, dx) }
            Flagged -> { state | dir = (-dx, -dy) }


infect2 : State -> State
infect2 state = 
    let 
        newNodeState = 
            case Maybe.withDefault Clean (Dict.get state.pos state.infected) of
                Clean -> Weakened
                Weakened -> Infected
                Infected -> Flagged 
                Flagged -> Clean
        
        newInfectionCount = 
            if newNodeState == Infected then 
                state.infectionCount + 1
            else 
                state.infectionCount
    in
        { state | infected = Dict.insert state.pos newNodeState state.infected, infectionCount = newInfectionCount}




step2 : State -> State
step2 state =
    state
        |> turn2
        |> infect2
        |> move

-- common stuff

move : State -> State
move state = 
    let 
        (x, y) = state.pos
        (dx, dy) = state.dir
    in
            { state | pos = (x + dx , y + dy) }

repeat : Int -> (State -> State) -> State -> State
repeat n step state = 
    if n == 0 then
        state
    else 
        repeat (n-1) step (step state) 

parseInput : String -> Infected
parseInput input = 
    String.lines input
        |> List.indexedMap (,)
        |> List.map parseLine
        |> List.concat
        |> Dict.fromList

parseLine : (Int, String) -> List (Pos, Node)
parseLine (y, line) = 
    String.toList line 
        |> List.indexedMap (\ x ch -> ((x,y),if ch == '#' then Infected else Clean))

