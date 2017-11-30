module AdventOfCode.View exposing ( view )

import AdventOfCode.Puzzle exposing (..)
import AdventOfCode.Model exposing (..)

import RemoteData exposing (WebData,RemoteData(..))

import Html exposing (Html, table, tr, td, th, thead, tbody, h1, h2, a, p, pre, div, text, input, button, program)
import Html.Attributes exposing (href, align)
import Html.Events exposing (onClick)

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
            Just (_,_,_,_,testRunner) -> testTable testRunner
            _ -> div [] [text "No testrunner defined for this day."]
    ]

testRow : TestResult -> Html Msg
testRow (ok, desc) = 
    tr [] [ 
        td [] [text desc],
        td [] [text (toString ok)]
    ]


puzzleTitle : Int -> Int -> String -> String
puzzleTitle year day desc = 
    (toString year) 
    ++ ": Day " ++ (toString day)
    ++ " - " ++ desc 

solverPanel : Model -> Html Msg
solverPanel model = 

    case model.selected of
        Just (year,day,desc,solver,_) -> 
            p [] [
                h2 [] [text (puzzleTitle year day desc)],
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
        (year, day, desc, _, _) = puzzle
        hash = (toString year) ++ "-day-" ++ (toString day)
    in
        a [href ("#" ++ hash), onClick (PuzzleSelected puzzle) ] [text (puzzleTitle year day desc)]

puzzleMenu : List Puzzle -> Html Msg
puzzleMenu puzzles = 
    div [] 
    (
        List.intersperse (text " | ")
            (List.map puzzleLink puzzles)
    )
