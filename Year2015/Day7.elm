
module Year2015.Day7 exposing (solver, test)

import Dict exposing (Dict)
import Bitwise


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

value : Circuit -> Source -> Int
value c source =
    case source of
        Value n -> 
            n
        Wire id ->
            case Dict.get id c of
                Nothing -> 0
                Just source -> value c source
        Gate gate ->
            case gate of 
                And a b -> Bitwise.and 0xffff (Bitwise.and (value c a) (value c b))
                Or a b -> Bitwise.and 0xffff (Bitwise.or (value c a) (value c b))
                Not a -> Bitwise.and 0xffff (Bitwise.complement (value c a))
                RShift a n -> Bitwise.and 0xffff (Bitwise.shiftRightZfBy n (value c a)) 
                LShift a n -> Bitwise.and 0xffff (Bitwise.shiftLeftBy n (value c a))


solver : Solver
solver input = 
    let 
        circuit = parseCircuit input
        answer = value circuit (Wire "h")
    in 
        (toString answer) ++ "    " ++ (toString circuit)


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