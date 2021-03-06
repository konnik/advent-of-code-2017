module Puzzles.Year2017.Day12 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Dict exposing (Dict)
import List exposing (take, drop, filter, map)
import String exposing (words, lines)
import Set exposing (Set)

puzzle : Puzzle
puzzle = ( 2017, 12, "Digital Plumber", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5" == "6",  "Test part 1" )
    , ( part2 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5" == "2",  "Test part 2" )
    ]


type alias Id = Int
type alias Group = Set Int
type alias Programs = Dict Id (List Id) 

part1 : PuzzleSolver
part1 input = 
    findProgs (parseInput input) 0 Set.empty
        |> Set.size
        |> toString


part2 : PuzzleSolver
part2 input = 
    let 
        p = parseInput input
        ids = allIds p
        groups = findGroups p (Set.toList ids) Set.empty []
    in 
        List.length groups 
        |> toString


findGroups : Programs -> List Id -> Set Id -> List Group -> List Group
findGroups progs ids usedIds groups = 
    case ids of
        [] -> groups
        id::rest -> 
            if Set.member id usedIds then
                findGroups progs rest usedIds groups
            else
                let
                    group = (findProgs progs id Set.empty)
                    newGroups = group::groups
                    newUsedIds = Set.union usedIds group 
                in
                    findGroups progs rest newUsedIds newGroups

inGroup : Int -> List Group -> Bool
inGroup id groups = 
    let 
        allIds = List.foldl Set.union Set.empty groups
    in
        Set.member id allIds


allIds : Programs -> Set Id
allIds progs = 
    let
        keys = Set.fromList (Dict.keys progs)
    in 
        List.foldl (Set.union << Set.fromList) keys (Dict.values progs)
        -- List.foldl a Set.empty [[1,2,3], [2,7,8]]
  

findProgs : Programs -> Int -> Set Int -> Set Int
findProgs progs id found  = 
    case Set.member id found of
        True -> found
        False -> 
            case Dict.get id progs of
                Nothing -> found
                Just children -> 
                    List.foldl (findProgs progs) (Set.insert id found) children

parseInput : String -> Programs
parseInput input = 
    List.foldl parseLine Dict.empty (lines input)

-- 2 <-> 0, 3, 4
parseLine : String -> Programs -> Programs
parseLine line progs = 
    let
        stripped = words (removeSeparators line)
        program = toInt (Maybe.withDefault "" (List.head stripped))
        children = map toInt (drop 2 stripped)
    in
        Dict.insert program children progs

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

removeSeparators : String -> String
removeSeparators = String.filter (\ c -> c/= ',' && c /= '(' && c/= ')')

