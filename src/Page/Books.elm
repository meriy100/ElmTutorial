module Page.Books exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, map2)

type alias Book =
    { name : String
    , author : String
    }

type Model
    = Failure
    | Loading
    | Success Book


type Msg
    = MorePlease
    | GotBook (Result Http.Error Book)

init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getBook)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (Loading, getBook)
        GotBook result ->
            case result of
                Ok book ->
                    (Success book, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)

getBook: Cmd Msg
getBook =
    Http.get
        { url = "http://localhost:4000/book.json"
        , expect = Http.expectJson GotBook bookDecoder
        }
--postBook : Book -> Cmd Msg
--postBook book =
--    Http.post
--        { url = "http://localhost:4000/book.json"
--        , body = "{ \"name\" : \"name\" }"
--        , expect = Http.expectJson GotBook bookDecoder
--        }

bookDecoder : Decoder Book
bookDecoder =
    field "data" (map2 Book (field "name" string) (field "author" string) )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    div []
    [ h2 [] [text "Book"]
    , viewBook model
    ]

viewBook model =
    case model of
        Failure ->
            div []
            [ text "I could not load a random cat for some reason. "
            , button [ onClick MorePlease ] [ text "Try Again!" ]
            ]
        Loading ->
            text "Loading"
        Success book ->
            div []
            [ p [ ] [ text book.name ]
            , p [ ] [ text book.author ]
            ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
