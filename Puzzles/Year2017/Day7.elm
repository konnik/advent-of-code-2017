module Puzzles.Year2017.Day7 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)
import Set
import Tuple exposing (first, second)
import List.Extra as LE


puzzle : Puzzle
puzzle = ( 2017, 7, "Recursive Circus", tests, part1, part2 )

tests : TestSuite
tests = 
    [ (part1 testInput == "tknk", "part 1")
    , (part2 testInput == "60", "part 2")
    , ( parseLine "pbga (66)" == Line "pbga"  66  [],  "parseLine - no children" )
    , ( parseLine "fwft (72) -> ktlj, cntj, xhth" == Line "fwft" 72 ["ktlj", "cntj", "xhth"],  "parseLine - children" )
    ]

testInput : String
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
cntj (57)"""

testData : Input
testData = 
        parseInput testInput

type alias Input = Dict String Line
type alias Line = {name: String, weight: Int, children: List String}

type Tree = Tree String Int (List Tree)

part1 : PuzzleSolver
part1 input = 
    input
    |> parseInput
    |> findRoot

part2 : PuzzleSolver
part2 input =
    let 
        data = parseInput input
        root = findRoot data
    in
        buildTree data root
        |> find
        |> List.map adjustWeight
        |> head
        |> toString    


findRoot : Input -> String
findRoot input = 
    let
        allChildren = List.concatMap .children (Dict.values input)
        allNames = Dict.keys input
        roots = List.filter (notInList allChildren) allNames
    in
        case roots of 
            root::[] -> root
            [] -> "No root found: " ++ (toString roots)
            _ -> "To many roots found: " ++ (toString roots)

notInList : List String -> String -> Bool
notInList list name = not (List.member name list)

-- part 2

buildTree : Input -> String -> Tree
buildTree input name = 
    case Dict.get name input of
        Nothing -> Tree ("Not found: " ++ name) 0 []
        Just line -> Tree line.name line.weight (List.map (buildTree input) line.children)

towerByName :  Dict String Line -> String -> Maybe Line
towerByName dict name = Dict.get name dict

stackWeight : Tree -> Int
stackWeight (Tree _ weight children) = 
    weight + List.sum (List.map stackWeight children)

isBalanced : Tree -> Bool
isBalanced (Tree _ weight children) = 
    Set.size (Set.fromList (List.map stackWeight children)) < 2

unbalancedWithBalancedChildren : Tree -> Bool
unbalancedWithBalancedChildren tree =
    (not (isBalanced tree)) && (List.all isBalanced (children tree))


find : Tree -> List Tree
find tree = 
    if unbalancedWithBalancedChildren tree then
        [tree]
    else
        List.concatMap find (children tree)


adjustWeight : Tree -> Int
adjustWeight  tree = 
    let 
        (Tree _ _ children) = tree
        (wrongWeight, correctWeight) = wrongStackWeight (List.map stackWeight children )
        wrongNodes = List.filter (hasStackWeight wrongWeight) children 
    in
        List.sum (List.map nodeWeight wrongNodes)
         |> (+) (correctWeight-wrongWeight)

nodeWeight : Tree -> Int
nodeWeight (Tree _ weight _ ) = weight

hasStackWeight : Int  -> Tree -> Bool
hasStackWeight weight ((Tree _ _ children) as tree) = stackWeight tree == weight

wrongStackWeight : List Int -> (Int, Int)
wrongStackWeight weights = 
    List.sort weights
        |> LE.group 
        |> List.map (\ x -> (head x , List.length x))
        |> List.sortBy second 
        |> List.map first
        |> listToTuple

     
children : Tree -> List Tree
children (Tree _ _ children) = children 

parseInput : String -> Input
parseInput input = 
    String.lines input 
        |> List.map parseLine 
        |> List.map nameAsKey 
        |> Dict.fromList

nameAsKey : Line -> (String, Line)
nameAsKey ({name} as line) = (name, line)

removeSeparators : String -> String
removeSeparators = String.filter (\ c -> c/= ',' && c /= '(' && c/= ')')

parseLine : String -> Line 
parseLine line =
    case String.words (removeSeparators line) of
        name :: weight :: [] -> 
            { name = name, weight = (toInt weight), children = [] }
        name :: weight :: arrow :: children ->
            { name = name, weight = (toInt weight), children = children }
        _ -> 
            { name = "Parse error: '" ++ line ++ "'", weight = 0, children = [] }

--  utillity functions 

listToTuple : List Int -> (Int, Int)
listToTuple list = 
    case list of 
        [a,b] -> (a,b)
        _ -> (0,0)

head : List Int -> Int
head = Maybe.withDefault 0 << List.head

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

