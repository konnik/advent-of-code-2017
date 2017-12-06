module Puzzles.Year2017.Day6 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List.Extra as LE

puzzle : Puzzle
puzzle = ( 2017, 6, "Memory Reallocation", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "0 2 7 0" == "5",  "Test part 1" )
    , ( part2 "0 2 7 0" == "4",  "Test part 2" )
    ]

type alias Banks = List Int
type alias State = 
    { banks: Banks
    , history: List Banks
    , loopLength: Int
    }

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> runUntil infiniteLoop
        |> .history
        |> List.length
        |> toString

part2 : PuzzleSolver
part2 input =
    parseInput input
        |> runUntil infiniteLoop
        |> .loopLength
        |> toString

parseInput : String -> State
parseInput input =
    { banks = input |> String.words |> List.map toInt
    , history = []
    , loopLength = 0
    }

infiniteLoop : State -> Bool
infiniteLoop state = List.member state.banks state.history

runUntil : (State -> Bool) -> State -> State
runUntil done state = 
        let
            newBanks = reallocateBlocks state.banks
            newState = { state | banks = newBanks, history = state.banks :: state.history }
        in
            case done newState of
                True -> { newState 
                        | loopLength = loopLength newState }
                False -> runUntil done newState

loopLength : State -> Int
loopLength state = 
    1 + Maybe.withDefault 0 (LE.elemIndex state.banks state.history)

reallocateBlocks : Banks -> Banks
reallocateBlocks banks = 
    let
        bank = mostBlocks banks
        blocks = blocksOfBank bank banks
    in
        banks
            |> zeroBank bank
            |> rotateLeft (bank + 1)
            |> distributeBlocks blocks
            |> rotateRight (bank + 1)
        
zeroBank : Int -> Banks -> Banks
zeroBank bank banks = Maybe.withDefault banks (LE.setAt bank 0 banks)

blocksOfBank : Int-> Banks -> Int
blocksOfBank bank banks = 
    Maybe.withDefault 0 (LE.getAt bank banks)

mostBlocks : Banks -> Int
mostBlocks banks = 
    let 
        max = Maybe.withDefault 0 (List.maximum banks)
    in
        Maybe.withDefault 0 (LE.elemIndex max banks)

distributeBlocks : Int -> Banks -> Banks
distributeBlocks blocks banks =
    case blocks of
        0 -> banks
        _ -> banks 
            |> insertOne
            |> rotateLeft 1
            |> distributeBlocks (blocks-1)
            |> rotateRight 1

insertOne : Banks -> Banks
insertOne banks =
    case banks of
        [] -> banks
        x::xs -> (x + 1) :: xs



--  utillity functions 

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

rotateLeft : Int -> List a -> List a
rotateLeft n list = 
    let
        n2 = n % (List.length list)
    in 
        (List.drop n2 list) ++ (List.take n2 list)

rotateRight : Int -> List a -> List a
rotateRight n list = 
    let
        n2 = n % (List.length list)
    in 
        (List.drop ((List.length list )-n2) list) ++ (List.take  ((List.length list )-n2) list)

