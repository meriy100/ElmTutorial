module Page.TextField exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "" }


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] [ text (String.reverse model.content) ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
