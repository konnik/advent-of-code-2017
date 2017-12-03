module AdventOfCode.View exposing ( view )

import AdventOfCode.Puzzle exposing (..)
import AdventOfCode.Model exposing (..)

import RemoteData exposing (WebData,RemoteData(..))

import Html exposing (Html, table, tr, td, th, thead, tbody, h1, h2, a, p, span, pre, div, text, input, button, program)
import Html.Attributes exposing (href, align, style)
import Html.Events exposing (onClick)

import Set

view : Model -> Html Msg
view model = 
    div [] 
    [ title "Advent of Code 2017 in Elm"
    , puzzleIndex model
    , puzzleContainer model
    ]


puzzleContainer : Model -> Html Msg
puzzleContainer model = 
    case model.selected of
        Nothing -> 
            text "Select a puzzle above..."
        _ -> 
            div [] 
                [ solverPanel model
                , testPanel model
                , inputPanel model 
                , debugPanel model
                ]

testTable : List TestResult -> Html Msg 
testTable testResults = 
    p [] [
        table [] [
            thead [] [
                tr [] [
                    th [align "left"] [text "Test"],
                    th [align "left"] [ text "Result"]
                ]
            ],
            tbody [] (List.map testRow testResults)
        ]
    ]

testSummary : List TestResult -> Html Msg
testSummary tests = 
    let
        failedTests = List.length (List.filter (\ (ok, _) -> ok == False) tests) 
    in
        case failedTests of 
            0 -> p [ style [("color", "green")]] [ text "All tests are green!" ]
            _ -> p [ style [("color", "red")]] [ text ("There are " ++ (toString failedTests) ++ " test failures.") ]

testPanel : Model -> Html Msg
testPanel model = 
    p [] [
        h2 [] [ text "Tests"],
        case model.selected of  
            Just (_,_,_,tests, _,_) ->
                div [] [
                    testSummary tests, 
                    testTable tests
                ]
            _ -> div [] [text "Select a puzzle to run tests"]
    ]

testRow : TestResult -> Html Msg
testRow (ok, desc) = 
    tr [] [ 
        td [] [text desc],
        td [] [testStatus ok]
    ]

testStatus : Bool -> Html Msg
testStatus result = 
    case result of
        True -> span [] [text "OK"]
        False -> span [style [("color", "red")]] [text "FAILED"]

puzzleTitle : Int -> Int -> String -> String
puzzleTitle year day desc = 
    (toString year) 
    ++ ": Day " ++ (toString day)
    ++ " - " ++ desc 

solverPanel : Model -> Html Msg
solverPanel model = 
    case model.selected of
        Just (year,day,desc,_,part1,part2) -> 
            p [] [
                h2 [] [text (puzzleTitle year day desc)],
                button [onClick (SolvePuzzle part1 (input model.input))] [text "Solve part 1"],
                button [onClick (SolvePuzzle part2 (input model.input))] [text "Solve part 2"],
                case model.answer of
                    Nothing -> text ""
                    Just answer -> 
                        p [] [ text ("Answer: " ++ answer)]
                ]
        Nothing -> 
            p [] [
                h2 [] [text "Puzzle"],
                text "Select a puzzle from menu."
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


puzzleForYearAndDay : List Puzzle -> Int -> Int -> Maybe Puzzle
puzzleForYearAndDay puzzles year day = 
    puzzles 
        |> List.filter (\ (y, d, _ , _, _, _) -> y == year && d == day )
        |> List.head 




puzzleLink : List Puzzle -> Int -> Int -> Html Msg
puzzleLink allPuzzles year day =
    let 
        commonStyle = 
            [ ("height","20px")
            , ("width","20px")
            , ("display", "inline-block")
            , ("text-align","center")
            , ("vertical-align", "middle")
            , ("padding", "5px")
            , ("text-decoration", "none")
            ]

        clickable = style (List.append [("font-weight", "bold"), ("color","yellow"), ("background-color","green")] commonStyle)
        notClickable = style (List.append [] commonStyle)
        hash = (toString year) ++ "-day-" ++ (toString day)
    in
        case puzzleForYearAndDay allPuzzles year day of
            Nothing -> span [notClickable] [text (toString day)]
            Just ((year, day, _ , _, _, _) as p) -> 
                a [href ("#" ++ hash), onClick (PuzzleSelected p), clickable ] [text (toString day)]

uniqueYears : List Puzzle -> List Int
uniqueYears puzzles = 
    puzzles
        |> List.map puzzleYear
        |> Set.fromList
        |> Set.toList

puzzleIndex : Model -> Html Msg
puzzleIndex model = 
    pre [] ( 
        [2015, 2016, 2017]
            |> List.map (puzzlesOfYear model.puzzles)
        )

isSolved : Int -> Int -> List Puzzle -> Bool
isSolved year day puzzles = 
    (List.length (List.filter (\(y, d, _,_,_,_) -> year == y && day == d) puzzles)) > 0


puzzlesOfYear : List Puzzle -> Int -> Html Msg
puzzlesOfYear puzzles year = 
    div [] (
            text (toString year) 
            :: text ": " 
            :: (List.range 1 25
                |> List.map (puzzleLink puzzles year)
                |> List.intersperse (text " ")
            )
        )

puzzleYear : Puzzle -> Year
puzzleYear (year,_,_,_,_,_) = year    

byYear : Int -> Puzzle -> Bool
byYear filterYear puzzle = 
    let 
        (year,_,_,_,_,_) = puzzle
    in
        year == filterYear 
