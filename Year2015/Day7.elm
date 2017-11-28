
module Year2015.Day7 exposing (solver, test)

import Dict exposing (Dict)
import Bitwise


import Types exposing (Solver, TestRunner, TestResult)


type alias Destination = String
type alias Id = String

type ValueOrWire 
    = Value Int 
    | Wire Id


type Gate 
    = And ValueOrWire ValueOrWire
    | Or ValueOrWire ValueOrWire
    | Not ValueOrWire
    | RShift ValueOrWire Int
    | LShift ValueOrWire Int

type Source 
        = ValueOrWire ValueOrWire
        | Gate Gate

type alias Connection = (Id, Source)

type alias Circuit = Dict Id Source


test : TestRunner 
test = 
    [ (int "78" == 78, "int - ska returnera talet")
    , (int "x" == 0, "int - ska returnera 0 som default")
    , (valueOrWire "xx" == Wire "xx", "valueOrWire - ska mappa 'xx' till Wire xx")
    , (valueOrWire "7" == Value 7, "valueOrWire - ska mappa '7' till Value 7")
    ]

int : String -> Int
int n = Result.withDefault 0 (String.toInt n)

valueOrWire : String -> ValueOrWire
valueOrWire str = 
  case String.toInt str of
    Ok n -> Value n
    Err _ -> Wire str

parseLine : String -> Maybe Connection
parseLine line = 
    let 
        tokens = String.words line
    in 
        case tokens of 
            a::"->"::w::[] -> Just (w, ValueOrWire (valueOrWire a))
            a::"AND"::b::"->"::w::[] -> Just (w, Gate (And (valueOrWire  a) (valueOrWire b)))
            a::"OR"::b::"->"::w::[] -> Just (w, Gate (Or (valueOrWire  a) (valueOrWire b)))
            a::"LSHIFT"::b::"->"::w::[] -> Just (w, Gate (LShift (valueOrWire  a) (int b)))
            a::"RSHIFT"::b::"->"::w::[] -> Just (w, Gate (RShift (valueOrWire  a) (int b)))
            "NOT"::a::"->"::w::[] -> Just (w, Gate (Not (valueOrWire  a)))
            _ -> Nothing

parseCircuit : String -> Circuit
parseCircuit input = 
    String.lines input
        |> List.filterMap parseLine
        |> Dict.fromList

value : Circuit -> ValueOrWire -> Int
value c val =
    case val of
        Value n -> n
        Wire id -> 
            case Dict.get id c of 
                Just (ValueOrWire w) -> value c w
                Just (Gate (And a b)) -> Bitwise.and 0xffff (Bitwise.and (value c a) (value c b))
                Just (Gate (Or a b)) -> Bitwise.and 0xffff (Bitwise.or (value c a) (value c b))
                Just (Gate (Not a )) -> Bitwise.and 0xffff (Bitwise.complement (value c a))
                Just (Gate (RShift w n )) -> Bitwise.and 0xffff (Bitwise.shiftRightZfBy n (value c w)) 
                Just (Gate (LShift w n )) -> Bitwise.and 0xffff (Bitwise.shiftLeftBy n (value c w))
                Nothing -> 0xff


solver : Solver
solver input = 
    let 
        circuit = parseCircuit input
        answer = value circuit (Wire "a")
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