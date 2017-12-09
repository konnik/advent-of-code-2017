module Puzzles.Year2017.Day9 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

puzzle : Puzzle
puzzle = ( 2017, 9, "Stream Processing", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( fst (parseElement (tokenize "{}")) == Group [],  "One group" )
    , ( fst (parseElement (tokenize "{{{}}}")) == Group [Group [Group []]],  "Tree groups" )
    , ( fst (parseElement (tokenize "{{},{}}")) == Group [Group [], Group []],  "Tree groups" )
    , ( fst (parseElement (tokenize "{<{},{},{{}}>}")) == Group [Garbage "{},{},{{}}"],  "Group with garbage" )
    , ( fst (parseElement (tokenize "{<a>,<a>,<a>,<a>}")) == Group [Garbage "a", Garbage "a", Garbage "a", Garbage "a"],  "Group with more garbage" )
    , ( fst (parseElement (tokenize "{{<a>},{<a>},{<a>},{<a>}}")) == Group [Group [Garbage "a"], Group [Garbage "a"], Group [Garbage "a"], Group [Garbage "a"]],  "Group with even more garbage" )
    , ( fst (parseElement (tokenize "{<{o\"i!a,<{i<a>}")) == Group [Garbage "{o\"i,<{i<a"],  "Garbage with '!'s" )
    , ( score 1 (fst (parseElement (tokenize "{}"))) == 1,  "Score 1" )
    , ( score 1 (fst (parseElement (tokenize "{{{}}}"))) == 6,  "Score 6" )
    , ( score 1 (fst (parseElement (tokenize "{{},{}}"))) == 5,  "Score 5" )
    ]
type alias Tokens = List Token
type alias Token = Char 

type Element 
    = Garbage String
    | Group (List Element)
    | Err String


part1 : PuzzleSolver
part1 input = 
    score 1 (fst (parseElement (tokenize input)))
    |> toString

part2 : PuzzleSolver
part2 input = 
    countGarbage (fst (parseElement (tokenize input)))
    |> toString

countGarbage : Element -> Int
countGarbage elem = 
    case elem of
        Group children -> List.sum (List.map countGarbage children)
        Garbage str -> String.length str
        _ -> 0

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

score : Int -> Element -> Int
score level elem = 
    case elem of
        Group children -> level + List.sum (List.map (score (level+1)) children)
        _ -> 0

fst : (a, b) -> a
fst (a,b) = a

tokenize : String -> Tokens
tokenize input = String.toList input

next : Tokens -> (Token, Tokens) 
next tokens = 
    case tokens of
        [] -> ('.', [])
        token::rest -> (token, rest) 

parseElement : Tokens -> (Element, Tokens)
parseElement tokens =
    case tokens of 
        '{'::xs -> parseGroup xs 
        '<'::xs -> parseGarbage xs
        _ -> (Err "Error parsing element: expecting '<' or '{'.", tokens)


parseList : Tokens -> (List Element, Tokens)
parseList tokens = 
    case next tokens of 
        ('}', rest) -> ([], rest)
        (',', rest) -> parseList rest
        _ -> 
            let 
                (elem, rest) = parseElement tokens
                (elemList, rest2 ) = parseList rest
            in
                (elem :: elemList, rest2 )  

parseGroup : Tokens -> (Element, Tokens)
parseGroup tokens = 
    case next tokens  of
        ( '}', rest ) -> (Group [], rest)
        ( _  , rest ) ->
            let 
                (children, nextRest) = parseList tokens
            in 
               (Group children, nextRest) 


parseGarbage : Tokens -> (Element, Tokens)
parseGarbage tokens = 
    let
        ( x  , rest ) = stripGarbage [] tokens 
    in 
        (Garbage (String.fromList (List.reverse x)) , rest)

stripGarbage : List Char -> Tokens -> (List Char, Tokens)
stripGarbage stripped tokens =
    case next tokens  of
        ( '>', rest ) -> (stripped, rest)
        ( '!', rest ) -> stripGarbage stripped (skipOne rest)
        ( tok, rest ) -> stripGarbage (tok::stripped) rest

skipOne : Tokens -> Tokens
skipOne tokens = 
    case tokens of 
        [] -> []
        _::rest -> rest
