module Puzzles.Year2017.Day23 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Dict exposing (Dict)
import List.Extra exposing (getAt)

import Arithmetic exposing (isPrime)

puzzle : Puzzle
puzzle = ( 2017, 23, "Coprocessor Conflagration", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( parseLine "set x 5" == Set "x" (Value 5),       "set x 5" )
    , ( parseLine "set x y" == Set "x" (Register "y"),  "set x y" )
    , ( parseLine "sub x 5" == Sub "x" (Value 5),       "sub x 5" )
    , ( parseLine "sub x y" == Sub "x" (Register "y"),  "sub x y" )
    , ( parseLine "mul x 5" == Mul "x" (Value 5),       "mul x 5" )
    , ( parseLine "mul x y" == Mul "x" (Register "y"),  "mul x y" )
    , ( parseLine "jnz 3 5" == Jnz (Value 3) (Value 5),           "jnz 3 5" )
    , ( parseLine "jnz x y" == Jnz (Register "x") (Register "y"), "jnz x y" )
--    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type Source 
    = Register String
    | Value Int

type Instruction
    = Set String Source
    | Sub String Source
    | Mul String Source
    | Jnz Source Source
    | Error String

type alias Registers = Dict String Int

type ProgramState = Running | Terminated

type alias State = 
    { registers: Registers
    , program: List Instruction
    , pos: Int
    , programState: ProgramState
    , mulCount: Int
    }

part1 : PuzzleSolver
part1 input = 
    let 
        initialState = 
            { registers = Dict.empty
            , program = parseInput input
            , pos = 0
            , programState = Running
            , mulCount = 0
            }
    in
        runUntilTerminated initialState
            |> .mulCount
            |> toString

part2 : PuzzleSolver
part2 input = 
    countNotPrimes 105700 122700 17    
    |> toString


{-

pseudocode for the algorithm (counting numbers that are not primes)

notPrimes = 0
for b = 105700 to 122700 step 17
    isPrime = true
    for d = 2 to b 
        for e = 2 to b 
            if b == e*d then 
                isPrime = false
    
    if not isPrime then
        notPrimes = notPrimes + 1 
-}

countNotPrimes : Int -> Int -> Int -> Int
countNotPrimes from to step = 
    if from > to then
        0
    else
        if isPrime from then
            0 + (countNotPrimes (from + step ) to step)
        else
            1 + (countNotPrimes (from + step ) to step)


runUntilTerminated : State -> State
runUntilTerminated state = 
    if (state.programState /= Running) then
        state
    else
        runUntilTerminated (runInstruction state)

runInstruction : State -> State
runInstruction state = 
    if (state.programState == Terminated) then 
        state
    else if (state.pos < 0) || (state.pos >= List.length state.program) then
        { state | programState = Terminated }
    else 
        let 
            instr = Maybe.withDefault (Error "outside program") (getAt state.pos state.program)
            valueOf src =   
                case src of 
                    Value x -> x
                    Register x -> Maybe.withDefault 0 (Dict.get x state.registers)
            set r v = Dict.insert r v state.registers
        in   
            case instr of
                Set reg source -> 
                    { state | pos = state.pos + 1, registers = set reg (valueOf source) }
                Sub reg source -> 
                    { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) - (valueOf source)) }
                Mul reg source -> 
                    { state | pos = state.pos + 1, registers = set reg ((valueOf (Register reg)) * (valueOf source)) , mulCount = state.mulCount + 1}
                Jnz x y -> 
                    if (valueOf x) /= 0 then 
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
            ["set",a,b] -> Set a (parseSource b)
            ["sub",a,b] -> Sub a (parseSource b)
            ["mul",a,b] -> Mul a (parseSource b)
            ["jnz",a,b] -> Jnz (parseSource a) (parseSource b)
            _ -> Error line

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt
