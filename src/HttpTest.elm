module HttpTest exposing (..)
import Browser
import Html exposing (Html, text, pre)
import Http

type Model
    = Failure
    | Loading
    | Success String

type Msg
    = GotText (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:4000"
        , expect = Http.expectString GotText
        }
    )
update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    (Success fullText, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."
        Loading ->
            text "Loading..."
        Success fullText ->
            pre [] [ text fullText ]

main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }