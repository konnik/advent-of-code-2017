module Puzzles.Year2017.Day7 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)
import Set



puzzle : Puzzle
puzzle = ( 2017, 7, "Recursive Circus", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( parseLine "pbga (66)" == ("pbga", 66, []),  "parseLine - no children" )
    , ( parseLine "fwft (72) -> ktlj, cntj, xhth" == ("fwft", 72, ["ktlj", "cntj", "xhth"]),  "parseLine - children" )
    , ( valueOf testDict "ugml" == 251,  "ugml - weight" )
    , ( valueOf testDict "padx" == 243,  "padx - weight" )
    , ( valueOf testDict "fwft" == 243,  "fwft - weight" )
    , ( balanced testDict "ugml" == True,  "ugml - balanced" )
    , ( balanced testDict "padx" == True,  "padx - balanced" )
    , ( balanced testDict "fwft" == True,  "fwft - balanced" )
    , ( balanced testDict "ktlj" == True,  "ktlj - balanced" )
    ]

testInput = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""

testDict = 
        String.lines testInput
            |> List.map parseLine
            |> toDict

type alias Tower = (String, Int, List String)

part1 : PuzzleSolver
part1 input = 
    "not implemented"

part2 : PuzzleSolver
part2 input =
    let 
        dict = 
        String.lines input
            |> List.map parseLine
            |> toDict
    in
       (toString (find dict "ykpsek" )) ++  
       (toString (List.map (valueOf dict) (children dict "uduyfo"))) ++ (toString (towerByName dict "cumah")) 

towerByName :  Dict String Tower -> String -> Maybe Tower
towerByName dict name = Dict.get name dict

unbalancedWithBalancedChildren : Dict String Tower -> String -> Bool
unbalancedWithBalancedChildren dict name = 
    (not (balanced dict name)) && (List.all (balanced dict) (children dict name))

find : Dict String Tower -> String -> List String
find dict name = 
    if unbalancedWithBalancedChildren dict name then
        [name]
    else
        List.concatMap (find dict) (children dict name)

children : Dict String Tower -> String -> List String
children dict name = 
    case Dict.get name dict of
        Nothing -> []
        Just (_,_,children) -> children

balanced : Dict String Tower -> String -> Bool
balanced dict name = 
    Set.size (Set.fromList (List.map (valueOf dict) (children dict name))) < 2

valueOf : Dict String Tower -> String -> Int
valueOf dict name = 
    case Dict.get name dict of
        Nothing -> 0
        Just (_,value,children) -> value + (List.sum (List.map (valueOf dict) children))

valueOfChildren : Dict String Tower -> String ->  Int
valueOfChildren dict name  = 
    case Dict.get name dict of
        Nothing -> 0
        Just (_,_,children) -> List.sum (List.map (valueOf dict) children)

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

