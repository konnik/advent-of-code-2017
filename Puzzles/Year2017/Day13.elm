module Puzzles.Year2017.Day13 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Dict exposing (Dict)
import List exposing (take, drop, filter, map)
import String exposing (words, lines)

puzzle : Puzzle
puzzle = ( 2017, 13, "Packet Scanners", tests, part1, part2 )

testInput = "0: 3\n1: 2\n4: 4\n6: 4"

tests : TestSuite
tests = 
    [
        ( part1 testInput == "24",  "Test part 1" )
  , ( part2 testInput == "10",  "Test part 2" )
    ]

type alias Scanner = { depth: Int, range: Int, position: Int,  direction: Int}
type alias Firewall = Dict Int Scanner
type alias State = { caught: Bool, position: Int, firewall: Firewall, severity: Int}

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> initialState
        |> stepUntil (pos 100)
        |> .severity
        |> toString
    
part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> Dict.values
        |> List.map toSimpleScanner
        |> findDelay 0 
        |> toString

scanners : String -> List (Int, Int)
scanners input = 
    parseInput input
        |> Dict.values
        |> List.map toSimpleScanner

findDelay : Int -> List (Int, Int) -> Int
findDelay delay scanners = 
    if canPass delay scanners then
        delay
    else
        findDelay (delay+1) scanners


toSimpleScanner : Scanner -> (Int, Int)
toSimpleScanner {depth, range} = (depth, range)

canPass : Int -> List (Int, Int) -> Bool
canPass delay scanners = 
    List.all (canPassScannerWithDelay delay) scanners

canPassScannerWithDelay : Int -> (Int, Int) -> Bool
canPassScannerWithDelay delay (depth, range) = 
    (delay + depth) % ((range-1)*2) /= 0

stepUntil : (State -> Bool) -> State -> State
stepUntil done state = 
    let 
        newState = 
            move state
            |> checkCaught
            |> moveScanners
    in
        if done newState then
            newState
        else
            stepUntil done newState

pos : Int ->  State -> Bool
pos pos state = state.position >= pos


moveScanners : State -> State
moveScanners state =
    {state | firewall = Dict.map moveScanner state.firewall }


moveScanner : Int -> Scanner -> Scanner 
moveScanner _ s = 
    let 
        newPos  = s.position + s.direction
        newDir = if newPos == 0 then
                    1
                else if newPos == (s.range-1) then
                    -1
                else
                    s.direction
    in 
        { s | direction = newDir, position  = newPos }

move : State -> State 
move state = 
    let 
        pos = state.position + 1
    in 
        { state | position = pos }

checkCaught : State -> State
checkCaught state = 
    if caught state then
        { state | caught = True, severity = state.severity + (severity state)}
    else
        state

severity : State -> Int 
severity state = 
    case scannerAt state.firewall state.position  of 
        Nothing -> 0
        Just scanner -> scanner.range * scanner.depth


caught : State -> Bool
caught state = 
    case scannerAt state.firewall state.position  of 
        Nothing -> False
        Just scanner -> scanner.depth == state.position && scanner.position == 0


scannerAt : Firewall -> Int -> Maybe Scanner
scannerAt firewall pos = Dict.get pos firewall 

initialState : Firewall -> State
initialState firewall = 
    { caught = False, position = -1, severity = 0, firewall = firewall}

parseInput : String -> Firewall
parseInput input = 
    Dict.fromList (map parseLine (lines input))

-- 2 <-> 0, 3, 4
parseLine : String  -> (Int, Scanner)
parseLine line = 
    case words (removeColon line) of
        [d, r] -> 
            let 
                depth = (toInt d)
                range = (toInt r)
                scanner = { depth = depth , range = range, position = 0, direction = 1}
            in
                (depth, scanner)
        _ -> 
            (-100, { depth = -100 , range = 0, position = 0, direction = 0} )


removeColon : String -> String
removeColon = String.filter (\ c -> c/= ':')

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

