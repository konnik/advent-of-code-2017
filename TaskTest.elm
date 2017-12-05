module TaskTest exposing (..)

import Html exposing (Html, text, ul, li)
import Time exposing (Time)
import Task exposing (Task, andThen, map)
import Date exposing (Date)

type alias Model = { messages: List String}

type Msg 
    = OnDate Date
    | OnTime Time
    | OnDateAndTime (Date, Time)
    | OnMessage String

init : (Model, Cmd Msg)
init = ( {messages = [" - Messages -"] }, getDateAndTime)

getDateAndTime : Cmd Msg
getDateAndTime = Task.perform OnDateAndTime (Task.map2 combine Date.now Time.now)

combine : a -> b -> (a, b)
combine a b = (a, b) 

getDate : Cmd Msg 
getDate = Task.perform OnDate Date.now

getTime : Cmd Msg 
getTime = Task.perform OnTime Time.now

view : Model -> Html Msg
view model = 
    ul [] 
        (List.map (\ m -> li [] [ text m ]) model.messages ) 

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

append : Model -> a -> Model
append model item = { model | messages = List.append model.messages [toString item] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        _ -> (append model msg) ! []

main : Program Never Model Msg
main = Html.program 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }