module Main exposing (main)

import Html exposing (Html, h1, a, pre, div, text, input, button, program)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

import Http
import RemoteData exposing (WebData,RemoteData(..))

type alias Solver = String -> String
type alias Id = String
type alias Name = String
type alias Puzzle = (Id, Name, Solver)
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
            [ ("test1", "Test1", dummySolver "test1")
            , ("test2", "Test2", dummySolver "test2")
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
    , inputPanel model 
    , debugPanel model
    ]

solverPanel : Model -> Html Msg
solverPanel model = 
    div [] [
        case model.input of 
            Success input -> 
                case model.selected of
                    Just (_,_,solver) -> 
                        button [onClick (SolvePuzzle solver input)] [text "Solve puzzle "]
                    Nothing -> 
                        div [] []
            _ -> 
                div [] []
    ,
    case model.answer of
        Nothing -> text ""
        Just answer -> text ("answer: " ++ answer)
    ]


inputPanel : Model -> Html Msg
inputPanel model = 
    case model.input of
        NotAsked -> div [] [text "Ingen input har laddats..."]
        Loading ->  div [] [text "Laddar..."]
        Failure err -> div [] [text ("Fel vid laddning: " ++ (toString err))]        
        Success input -> div [] [pre [] [text input]]

title : String -> Html Msg
title titleText = 
    h1 [] [text titleText]

debugPanel : Model -> Html Msg
debugPanel model = 
    div [] [text (toString { model | input = NotAsked})]



puzzleLink : Puzzle -> Html Msg
puzzleLink puzzle =
    let 
        (id, name, _) = puzzle
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
        (id, _, _) = puzzle
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

