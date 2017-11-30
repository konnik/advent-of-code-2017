
module Year2015.Day7 exposing (solver, test)

import Dict exposing (Dict)
import Bitwise
import Debug 


import Types exposing (Solver, TestRunner, TestResult)

type alias Destination = String
type alias Id = String

type Gate 
    = And Source Source
    | Or Source Source
    | Not Source
    | RShift Source Int
    | LShift Source Int

type Source 
        = Wire Id
        | Value Int
        | Gate Gate

type alias Connection = (Id, Source)

type alias Circuit = Dict Id Source


test : TestRunner 
test = 
    [ (int "78" == 78, "int - ska returnera talet")
    , (int "x" == 0, "int - ska returnera 0 som default")
    , (valueOrWire "xx" == Wire "xx", "valueOrWire - ska mappa 'xx' till Wire xx")
    , (valueOrWire "7" == Value 7, "valueOrWire - ska mappa '7' till Value 7")
    , (parseLine "19138 -> b" == Just ("b", Value 19138), "parseLine - value input")
    , (parseLine "ci RSHIFT 1 -> db" == Just ("db", Gate (RShift (Wire "ci") 1)), "parseLine - RSHIFT - wire")
    , (parseLine "234 RSHIFT 4 -> db" == Just ("db", Gate (RShift (Value 234) 4)), "parseLine - RSHIFT - value")
    , (parseLine "NOT tx -> yr" == Just ("yr", Gate (Not (Wire "tx"))), "parseLine - NOT - wire")
    ]

int : String -> Int
int n = Result.withDefault 0 (String.toInt n)

valueOrWire : String -> Source
valueOrWire str = 
  case String.toInt str of
    Ok n -> Value n
    Err _ -> Wire str

parseLine : String -> Maybe Connection
parseLine line = 
    case String.words line of 
        [a, "->", w] -> Just (w, valueOrWire a)
        [a, "AND", b, "->", w] -> Just (w, Gate (And (valueOrWire  a) (valueOrWire b)))
        [a, "OR", b, "->", w] -> Just (w, Gate (Or (valueOrWire  a) (valueOrWire b)))
        [a, "LSHIFT", b, "->", w] -> Just (w, Gate (LShift (valueOrWire  a) (int b)))
        [a, "RSHIFT", b, "->", w] -> Just (w, Gate (RShift (valueOrWire  a) (int b)))
        ["NOT", a, "->", w ] -> Just (w, Gate (Not (valueOrWire  a)))
        _ -> Nothing

parseCircuit : String -> Circuit
parseCircuit input = 
    String.lines input
        |> List.filterMap parseLine
        |> Dict.fromList



binaryOp : Circuit -> Source -> Source -> (Int -> Int -> Int) -> (Int, Circuit)
binaryOp c a b f = 
    let 
        (valueA, c1) = value c a
        (valueB, c2) = value c1 b
    in 
        (Bitwise.and 0xffff (f valueA valueB), c2)

unaryOp : Circuit -> Source -> (Int -> Int) -> (Int, Circuit)
unaryOp c a f = 
    let 
        (valueA, c1) = value c a
    in 
        (Bitwise.and 0xffff (f valueA ), c1)


cacheAndReturnValue : Circuit -> Id -> Int -> (Int, Circuit)
cacheAndReturnValue c id v = 
    (v, Dict.insert id (Value v) c)

value : Circuit -> Source -> (Int, Circuit)
value c source =
    case (Debug.log "value: " source) of
        Value n -> 
            (n, c)
        Wire id ->
            case Dict.get id c of
                Nothing -> Debug.log "NOTHING: " (0,c)
                Just newSource -> 
                    let
                        (v2, c2) = value c newSource
                    in
                        cacheAndReturnValue c2 id v2
        Gate gate ->
            case gate of 
                And a b -> binaryOp c a b Bitwise.and
                Or a b -> binaryOp c a b Bitwise.or
                Not a -> unaryOp c a Bitwise.complement
                RShift a n -> unaryOp c a (Bitwise.shiftRightZfBy n) 
                LShift a n -> unaryOp c a (Bitwise.shiftLeftBy n )


solver : Solver
solver input = 
    let 
        circuit = parseCircuit input
        (answer, circuit2) = value circuit (Wire "a")

        circuit3 = Dict.insert "b" (Value answer) circuit
        (answer2, circuit4) = value circuit3 (Wire "a")
    in 
        "Answer 1: "  ++ (toString answer) ++ ",    Answer 2: " ++ (toString answer2)


{- 
For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456
-}