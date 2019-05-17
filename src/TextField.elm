module Page.TextField exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { content : String
    }


zero p x = x
one p x = p x
two p x =  x |> p |> p
three p x =  x |> p |> p |> p
five = plus three two
increment n p x = p (n p x)
plus n m = n increment m

pair x y f = f x y
left p = \x -> \y -> x
right p = \x -> \y -> y

to_integer lambda = lambda (\n -> n + 1) 0

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
        , span [] [ one |> to_integer |> String.fromInt |> text ]
        , span [] [ two |> to_integer |> String.fromInt |> text ]
        , span [] [ three |> to_integer |> String.fromInt |> text ]
        , span [] [ three |> increment |> increment |> to_integer |> String.fromInt |> text ]
        , span [] [ (plus five two) |> to_integer |> String.fromInt |> text ]
        ]

main =
    Browser.sandbox { init = init, update = update, view = view }
