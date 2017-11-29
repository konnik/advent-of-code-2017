module Main exposing (main)

import Types exposing (..)

import Html exposing (Html, table, tr, td, th, thead, tbody, h1, h2, a, p, pre, div, text, input, button, program)
import Html.Attributes exposing (href, align)
import Html.Events exposing (onClick)

import Http
import RemoteData exposing (WebData,RemoteData(..))

import Year2015.Day7

type alias Id = String
type alias Name = String
type alias Puzzle = (Id, Name, Solver, Maybe TestRunner)
type alias PuzzleInput = String

type alias Model = { puzzles : List Puzzle,
                     selected : Maybe Puzzle,
                     input : WebData PuzzleInput,
                     answer : Maybe String }
type Msg 
    = NoOp 
    | PuzzleSelected Puzzle
    | OnPuzzleInputFetched Puzzle (WebData PuzzleInput)
    | SolvePuzzle Solver PuzzleInput

dummySolver : String ->  Solver  
dummySolver id input = id ++ ": " ++ (toString (String.length input))

init : ( Model, Cmd Msg )
init = ({ puzzles = 
            [ ("test1", "Test1", dummySolver "test1", Nothing)
            , ("test2", "Test2", dummySolver "test2", Nothing)
            , ("2015-day-7", "2015 Dag 7", Year2015.Day7.solver, Just Year2015.Day7.test)
            , ("2015-day-7-test", "2015 Dag 7 - test", Year2015.Day7.solver, Nothing)
            ]
        , selected = Nothing
        , input = NotAsked
        , answer = Nothing
        }, Cmd.none)

view : Model -> Html Msg
view model = 
    div [] 
    [ title "Advent of Code 2017 in Elm"
    , puzzleMenu model.puzzles
    , solverPanel model
    , testPanel model
    , inputPanel model 
    , debugPanel model
    ]


testTable : List TestResult -> Html Msg 
testTable testResults = 
    table [] [
        thead [] [
            tr [] [
                th [align "left"] [text "Test"],
                th [align "left"] [ text "Result"]
            ]
        ],
        tbody [] (List.map testRow testResults)
    ]

testPanel : Model -> Html Msg
testPanel model = 
    p [] [
        h2 [] [ text "Tests"],
        case model.selected of  
            Just (_,_,_,Just testRunner) -> testTable testRunner
            _ -> div [] [text "No testrunner defined for this day."]
    ]

testRow : TestResult -> Html Msg
testRow (ok, desc) = 
    tr [] [ 
        td [] [text desc],
        td [] [text (toString ok)]
    ]


solverPanel : Model -> Html Msg
solverPanel model = 

    case model.selected of
        Just (_,name,solver,_) -> 
            p [] [
                h2 [] [text ("Puzzle: " ++ name)],
                button [onClick (SolvePuzzle solver (input model.input))] [text "Solve puzzle "],
                case model.answer of
                    Nothing -> text ""
                    Just answer -> text ("Answer: " ++ answer)
                ]
        Nothing -> 
            p [] [
                h2 [] [text "Puzzle"],
                text "Select ap Puzzle from menu."
            ]


input : WebData String -> String
input input = 
    case input of
        Success txt -> txt
        _ -> ""

inputPanel : Model -> Html Msg
inputPanel model = 
    p [] [
        h2 [] [text "Input"],
        case model.input of
            NotAsked -> div [] [text "Ingen input har laddats..."]
            Loading ->  div [] [text "Laddar..."]
            Failure err -> div [] [text ("Fel vid laddning: " ++ (toString err))]        
            Success input -> div [] [pre [] [text input]]
    ]

title : String -> Html Msg
title titleText = 
    h1 [] [text titleText]

debugPanel : Model -> Html Msg
debugPanel model = 
    p [] [
        h2 [] [ text "Debug"],
        pre [] [text (toString { model | input = NotAsked})]
    ]


puzzleLink : Puzzle -> Html Msg
puzzleLink puzzle =
    let 
        (id, name, _, _) = puzzle
    in
        a [href ("#" ++ id), onClick (PuzzleSelected puzzle) ] [text name]

puzzleMenu : List Puzzle -> Html Msg
puzzleMenu puzzles = 
    div [] 
    (
        List.intersperse (text " | ")
            (List.map puzzleLink puzzles)
    )

fetchPuzzleInput : Puzzle -> Cmd Msg
fetchPuzzleInput puzzle = 
    let 
        (id, _, _, _) = puzzle
    in
        Http.getString ("inputs/" ++ id ++ ".txt")
            |> RemoteData.sendRequest
            |> Cmd.map (OnPuzzleInputFetched puzzle)

update : Msg -> Model -> (Model, Cmd Msg)
update cmd model = 
    case cmd of
        NoOp -> 
            model ! []
        PuzzleSelected puzzle -> 
            {model | answer = Nothing, selected = Nothing, input = NotAsked } ! [fetchPuzzleInput puzzle]
        OnPuzzleInputFetched puzzle webdata -> 
            {model | input = webdata, selected = Just puzzle} ! []
        SolvePuzzle solver input -> 
            {model | answer = Just (solver input) } ! []



subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

