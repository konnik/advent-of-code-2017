module Puzzles.Year2017.Day10 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List.Extra exposing (groupsOf)
import Bitwise

puzzle : Puzzle
puzzle = ( 2017, 10, "Knot Hash", tests, part1, part2 )

-- tests

tests : TestSuite
tests = 
    [ ( part1 "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110" == "23874",  "Test part 1" )
    , ( part2 "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110" == "e1a65bfb5a5ce396025fab5528c25a87",  "Test part 2" )
    ]

-- start of program

type alias State = {numbers: List Int, pos: Int, skipSize: Int }

initialState : State
initialState = {numbers = List.range 0 255, pos= 0, skipSize= 0}

part1 : PuzzleSolver
part1 input = 
    parseInputAsList input
        |> run initialState 
        |> .numbers
        |> multFirstTwo
        |> toString

part2 : PuzzleSolver
part2 input =
    (parseInputAsAscii input) ++ [17, 31, 73, 47, 23]
        |> repeat 64 run initialState 
        |> .numbers
        |> denseHash
        |> asHexString


parseInputAsList : String -> List Int
parseInputAsList input = List.map toInt (String.split "," input)

parseInputAsAscii : String -> List Int
parseInputAsAscii input = List.map ascii (String.toList input)



denseHash : List Int -> List Int
denseHash = List.map xorList << (groupsOf 16)

xorList : List Int -> Int
xorList = List.foldl Bitwise.xor 0 

repeat : Int -> (State -> List Int -> State) -> State -> List Int -> State
repeat n func state input = 
    case n of
        0 -> state
        _ -> repeat (n-1) func (func state input) input


run : State -> List Int -> State
run state input = 
    case input of
        [] -> state
        x::xs -> run (step state x) xs

multFirstTwo : List Int -> Int
multFirstTwo list  =
    case list of
        a::b::_ -> a*b
        _ -> -1


step : State -> Int -> State 
step state len = 
    let 
        rotList = rotateLeft state.pos state.numbers
        sublist = List.take len rotList
        reversed = List.reverse sublist
        rotList2 = reversed ++ List.drop len rotList

        newNumbers = rotateRight state.pos rotList2         
        newPos = (state.pos + len + state.skipSize) % List.length state.numbers
        newSkipSize = state.skipSize + 1
    in
        { numbers = newNumbers, pos = newPos, skipSize = newSkipSize }

reverseSubList : Int -> Int -> List Int -> List Int
reverseSubList start len list =
    let 
        rotList = rotateLeft start list
        reversed = (List.reverse (List.take len rotList)) ++ List.drop len rotList
        newList = rotateRight start reversed         
    in
        newList

ascii : Char -> Int
ascii ch = 
    case ch of 
        '0' -> 48
        '1' -> 49
        '2' -> 50
        '3' -> 51
        '4' -> 52
        '5' -> 53
        '6' -> 54
        '7' -> 55
        '8' -> 56
        '9' -> 57
        ',' -> 44 
        _ -> -1


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

asHexString : List Int -> String
asHexString bytes = 
    String.concat (List.map toHex bytes)

toHex : Int -> String
toHex n = 
    (hexDigit (n // 16)) ++ (hexDigit (n % 16))

hexDigit : Int -> String
hexDigit n = 
    case n of
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        10 -> "a"
        11 -> "b"
        12 -> "c"
        13 -> "d"
        14 -> "e"
        15 -> "f"
        _ -> "."