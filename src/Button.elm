module Button exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        num : Int
    }

init : Model
init = { num = 0 }

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | num = model.num + 1 }
        Decrement ->
            { model | num = model.num - 1 }

view : Model -> Html Msg
view model =
    div [] [
        button [onClick Decrement] [ "-" |> text],
        span [] [ model.num |> String.fromInt |> text ],
        button [onClick Increment] [ "+" |> text],
        button [onClick Decrement] [ "-" |> text],
        span [] [ model.num |> String.fromInt |> text ],
        button [onClick Increment] [ "+" |> text]
    ]
