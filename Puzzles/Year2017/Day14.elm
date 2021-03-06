module Puzzles.Year2017.Day14 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Puzzles.Year2017.Day10 exposing (..)

import List.Extra exposing (groupsOf)
import Bitwise
import Char
import Tuple exposing (first, second)
import Set exposing (Set, intersect, fromList, diff, union)


puzzle : Puzzle
puzzle = ( 2017, 14, "Disk Defragmentation", tests, part1, part2 )

tests : TestSuite
tests = 
    [ -- tests are too slow.. :-)
        -- ( part1 "test-input" == "expected-output",  "Test part 1" )
    --, ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type alias Pos = (Int, Int)

part1 : PuzzleSolver
part1 input = 
    List.range 0 127
        |> List.map (hashRow input) 
        |> List.map List.sum
        |> List.sum
        |> toString

part2 : PuzzleSolver
part2 input = 
    List.range 0 127
        |> List.map (hashRow input) 
        |> usedSquares
        |> countGroups 0
        |> toString


countGroups: Int -> Set Pos -> Int
countGroups n cells = 
    case Set.toList cells of
        [] -> n
        p :: _ -> countGroups (n+1) (diff cells (findGroup (Set.singleton p) cells))


expandWithNeighbours : Set Pos -> Set Pos
expandWithNeighbours cells = 
    Set.toList cells
        |> List.concatMap (\ (x,y) -> [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)]) 
        |> Set.fromList


findGroup : Set Pos -> Set Pos -> Set Pos
findGroup group cells = 
    let
        newGroup = intersect cells (expandWithNeighbours group)
    in
        if Set.size newGroup == Set.size group then 
            group
        else
            findGroup newGroup cells

    

usedSquares : List (List Int) -> Set Pos
usedSquares grid = 
    let
        ones (p, x) = x == 1
        indexedRows = List.indexedMap (,) grid
    in
       Set.fromList (List.map first (List.filter ones ( List.concat (List.map indexCols indexedRows))))

indexCols : (Int , List a) -> List ((Int, Int ), a)
indexCols (y, items) = 
    let 
        mapItem y x e = ((x,y), e)
    in
        List.indexedMap (mapItem y) items

countOnes : String -> Int
countOnes str = 
     List.filter (\ x-> x == '1') (String.toList str)
        |> List.length

hashRow : String -> Int -> List Int 
hashRow input row = knotHash (input ++ "-" ++ (toString row))

-- from day 10

byteToBits : Int -> List Int
byteToBits x =
    let
        bit x n = Bitwise.shiftRightBy n x |> Bitwise.and 0x1 
    in
        List.map (bit x) (List.reverse (List.range 0 7))


knotHash : String -> List Int
knotHash input =
    (parseInputAsAscii input) ++ [17, 31, 73, 47, 23]
        |> repeat 64 run initialState 
        |> .numbers
        |> denseHash
        |> toBits


toBits : List Int -> List Int
toBits bytes = 
    List.concatMap byteToBits bytes

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
ascii ch = Char.toCode ch


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
