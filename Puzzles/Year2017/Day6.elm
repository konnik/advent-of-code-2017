module Puzzles.Year2017.Day6 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List.Extra exposing (..)


puzzle : Puzzle
puzzle = ( 2017, 6, "CHANGE ME", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "test-input" == "expected-output",  "Test part 1" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]


type alias Banks = List Int

part1 : PuzzleSolver
part1 input = 
    input
        |> parseInput
        |> run []
        |> List.length
        |> (+) 1
        |> toString

part2 : PuzzleSolver
part2 input = 
    "not implemented"

distribute : Int-> Banks -> Banks
distribute num banks =
    case num of
        0 ->
            banks
        n ->
        case banks of
            b::bs ->
                right 1 (distribute (n-1) (left 1 ((b+1)::bs)))
            _ -> banks

left : Int -> List a -> List a
left n list = 
    (List.drop n list) ++ (List.take n list)

right : Int -> List a -> List a
right n list = 
     (List.drop ((List.length list )-n) list) ++ (List.take  ((List.length list )-n) list)


runIt : Banks -> List Banks
runIt banks = run [banks] banks

run : List Banks -> Banks -> List Banks
run list banks = 
        let
            banks2 = (distributeNext banks)
        in
            if List.member banks2 list then
                let a = Debug.log "Index:" (elemIndex banks2 list) in
                list

            else
                run (banks2::list) banks2

distributeNext : Banks -> Banks
distributeNext banks = 
    let
        index = nextBankToDistribute banks
        value = Maybe.withDefault 0 (getAt index banks)
        bank2 = Maybe.withDefault [] (setAt index 0 banks)
        rot = ((index +1) % 16)
        bank3 = left rot bank2
    in
        right rot (distribute value bank3)
        
        

nextBankToDistribute : Banks -> Int
nextBankToDistribute banks = 
    let 
        max = Maybe.withDefault 0 (List.maximum banks)
    in
        Maybe.withDefault 0 (elemIndex max banks)

rotateLeft : Int -> List a -> List a
rotateLeft n list = 
    (List.drop n list) ++ (List.take n list)

parseInput : String -> Banks
parseInput input =
    input
        |> String.words 
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)
