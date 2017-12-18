module Puzzles.Year2017.Day18 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)
import List.Extra exposing (getAt)

puzzle : Puzzle
puzzle = ( 2017, 18, "Duet", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( parseLine "snd 5" == Snd (Value 5),  "snd 5" )
    , ( parseLine "snd x" == Snd (Register "x"),  "snd x" )
    , ( parseLine "set x 5" == Set "x" (Value 5),       "set x 5" )
    , ( parseLine "set x y" == Set "x" (Register "y"),  "set x y" )
    , ( parseLine "add x 5" == Add "x" (Value 5),       "add x 5" )
    , ( parseLine "add x y" == Add "x" (Register "y"),  "add x y" )
    , ( parseLine "mul x 5" == Mul "x" (Value 5),       "mul x 5" )
    , ( parseLine "mul x y" == Mul "x" (Register "y"),  "mul x y" )
    , ( parseLine "mod x 5" == Mod "x" (Value 5),       "mod x 5" )
    , ( parseLine "mod x y" == Mod "x" (Register "y"),  "mod x y" )
    , ( parseLine "rcv 5"   == Rcv (Value 5),           "rcv 5" )
    , ( parseLine "rcv y"   == Rcv (Register "y"),      "rcv y" )
    , ( parseLine "jgz 3 5" == Jgz (Value 3) (Value 5),           "jgz 3 5" )
    , ( parseLine "jgz x y" == Jgz (Register "x") (Register "y"), "jgz x y" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type Source 
    = Register String
    | Value Int
type Instruction
    = Snd Source
    | Set String Source
    | Add String Source
    | Mul String Source
    | Mod String Source
    | Rcv Source
    | Jgz Source Source
    | Error String

type alias Registers = Dict String Int
type alias State = {registers: Registers, program: List Instruction, pos: Int, currentSound: Maybe Int, recoveredSound : Maybe Int}
type alias State2 = {registers: Registers, program: List Instruction, pos: Int, waits: Bool, terminated: Bool, sendCount: Int, rcvQueue: List Int, sndQueue: List Int}

part1 : PuzzleSolver
part1 input = 
    let 
        initialState = 
            { registers = Dict.empty
            , program = parseInput input
            , pos = 0
            , currentSound = Nothing
            , recoveredSound = Nothing
            }
    in
        runUntilRecovered initialState
            |> toString

part2 : PuzzleSolver
part2 input = 
    let 
        initialState0 = 
            { registers = Dict.insert "p" 0 Dict.empty
            , program = parseInput input
            , pos = 0
            , waits = False
            , terminated = False
            , sendCount = 0
            , rcvQueue = []
            , sndQueue = []
            }
        initialState1 = 
            { registers = Dict.insert "p" 1 Dict.empty
            , program = parseInput input
            , pos = 0
            , waits = False
            , terminated = False
            , sendCount = 0
            , rcvQueue = []
            , sndQueue = []
            }
    in
        runUntilEndOrDeadlock initialState0 initialState1
            |> toString

runUntilEndOrDeadlock : State2 -> State2 -> Int
runUntilEndOrDeadlock a b = 
    if (a.terminated || a.waits) && (b.terminated || b.waits) then
        b.sendCount
    else
        let 
            newStateA = runInstruction2 { a | rcvQueue = b.sndQueue }
            newStateB = runInstruction2 { b | rcvQueue = newStateA.sndQueue }
        in 
            runUntilEndOrDeadlock newStateA newStateB

runInstruction2 : State2 -> State2
runInstruction2 state = 
    let 
        instr = Maybe.withDefault (Error "outside program") (getAt state.pos state.program)
        valueOf src =   
            case src of 
                Value x -> x
                Register x -> Maybe.withDefault 0 (Dict.get x state.registers)
        set r v = Dict.insert r v state.registers
    in   
        case instr of
            Snd x -> 
                { state | pos = state.pos + 1, sndQueue = [x]}
            Set reg source -> 
                { state | pos = state.pos + 1, registers = set reg (valueOf source) }
            Add reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) + (valueOf source)) }
            Mul reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) * (valueOf source)) }
            Mod reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) % (valueOf source)) }
            Rcv reg -> 
                case state.rcvQueue of
                    [] -> { state | waits = True} 
                    x::rest -> { state | pos = state.pos + 1, rcvQueue = rest, set reg}
                if (valueOf x) /= 0 then 
                    { state | pos = state.pos + 1}
                else 
                    { state | pos = state.pos + 1 } 
            Jgz x y -> 
                if (valueOf x) > 0 then 
                    { state | pos = state.pos + (valueOf y) }
                else 
                    { state | pos = state.pos + 1 } 
            _ -> Debug.log ("Unhandled instr: " ++ (toString instr)) state


runUntilRecovered : State -> Int
runUntilRecovered state = 
    case state.recoveredSound of
        Just snd -> snd
        Nothing -> 
            runUntilRecovered (runInstruction state)

runInstruction : State -> State 
runInstruction state = 
    let 
        instr = Maybe.withDefault (Error "outside program") (getAt state.pos state.program)
        valueOf src =   
            case src of 
                Value x -> x
                Register x -> Maybe.withDefault 0 (Dict.get x state.registers)
        set r v = Dict.insert r v state.registers
    in   
        case instr of
            Snd x -> 
                { state | pos = state.pos + 1, currentSound = Just (valueOf x)}
            Set reg source -> 
                { state | pos = state.pos + 1, registers = set reg (valueOf source) }
            Add reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) + (valueOf source)) }
            Mul reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) * (valueOf source)) }
            Mod reg source -> 
                { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) % (valueOf source)) }
            Rcv x -> 
                if (valueOf x) /= 0 then 
                    { state | pos = state.pos + 1, recoveredSound = state.currentSound }
                else 
                    { state | pos = state.pos + 1 } 
            Jgz x y -> 
                if (valueOf x) > 0 then 
                    { state | pos = state.pos + (valueOf y) }
                else 
                    { state | pos = state.pos + 1 } 
            _ -> Debug.log ("Unhandled instr: " ++ (toString instr)) state

parseInput : String -> List Instruction
parseInput input = 
    String.lines input 
        |> List.map parseLine

parseLine : String -> Instruction
parseLine line =
    let 
        parseSource x = 
            case String.toInt x of
                Ok value -> Value value
                _ -> Register x
    in
        case String.split " " line of
            ["snd",a] -> Snd (parseSource a)
            ["set",a,b] -> Set a (parseSource b)
            ["add",a,b] -> Add a (parseSource b)
            ["mul",a,b] -> Mul a (parseSource b)
            ["mod",a,b] -> Mod a (parseSource b)
            ["rcv",a] -> Rcv (parseSource a)
            ["jgz",a,b] -> Jgz (parseSource a) (parseSource b)
            _ -> Error line

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt
