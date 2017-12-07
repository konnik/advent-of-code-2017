module Puzzles.Year2017.Day7 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)



puzzle : Puzzle
puzzle = ( 2017, 7, "Recursive Circus", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( parseLine "pbga (66)" == ("pbga", 66, []),  "parseLine - no children" )
    , ( parseLine "fwft (72) -> ktlj, cntj, xhth" == ("fwft", 72, ["ktlj", "cntj", "xhth"]),  "parseLine - children" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type alias Tower = (String, Int, List String)

part1 : PuzzleSolver
part1 input = 
    "not implemented"

part2 : PuzzleSolver
part2 input = 
    "not implemented"


firstAsKey : (a,b,c) -> (a, (a,b,c))
firstAsKey (a,b,c) = (a, (a,b,c))

toDict : List Tower -> Dict String Tower
toDict towers = 
    Dict.fromList (List.map firstAsKey towers)

parseLine : String -> Tower 
parseLine line =
    let 
        filteredLine 
            = String.filter (\ c -> c/= ',' && c /= '(' && c/= ')') line
    in
        case String.words filteredLine of
            [name, weight] -> (name, (toInt weight), [])
            name::weight::_::children -> (name, (toInt weight), children)
            _ -> ("ParseError", -1, [])

        --  utillity functions 

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

