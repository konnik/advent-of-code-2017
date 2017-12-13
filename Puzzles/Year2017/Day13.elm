module Puzzles.Year2017.Day13 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List exposing (take, drop, filter, map, sum)
import String exposing (words, lines)

puzzle : Puzzle
puzzle = ( 2017, 13, "Packet Scanners", tests, part1, part2 )

testInput : String
testInput = "0: 3\n1: 2\n4: 4\n6: 4"

tests : TestSuite
tests = 
    [ ( part1 testInput == "24",  "Test part 1" )
    , ( part2 testInput == "10",  "Test part 2" )
    ]

type alias Scanner = (Int, Int)

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> tripSeverity
        |> toString
    
part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> findDelay 0 
        |> toString

tripSeverity : List (Int, Int) -> Int
tripSeverity scanners = 
    let 
        severityOf ((depth, range) as scanner) = 
            case canPassScanner 0 scanner of
                True -> 0
                False -> range * depth
    in
       sum (map severityOf scanners)

findDelay : Int -> List (Int, Int) -> Int
findDelay delay scanners = 
    if canPassAllScanners delay scanners then
        delay
    else
        findDelay (delay+1) scanners


canPassAllScanners : Int -> List (Int, Int) -> Bool
canPassAllScanners delay scanners = 
    List.all (canPassScanner delay) scanners

canPassScanner : Int -> (Int, Int) -> Bool
canPassScanner delay (depth, range) = 
    (delay + depth) % ((range-1)*2) /= 0


parseInput : String -> List Scanner
parseInput input = 
    map parseLine (lines input)

parseLine : String  -> Scanner
parseLine line = 
    case words (removeColon line) of
        [depth, range] -> (toInt depth, toInt range)
        _ -> (999,0)

removeColon : String -> String
removeColon = String.filter (\ c -> c/= ':')

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

