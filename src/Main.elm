module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Entry as Entry exposing (Entry)

type Model
    = Failure String
    | Loading
    | Success (List Entry)

type Msg
    = GotEntries (Result Http.Error (List Entry))
    | PostedEntry (Result Http.Error String)
    | PostEntry

init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getEntries)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    (Success entries, Cmd.none)
                Err error ->
                    case error of
                        Http.BadBody message ->
                            (Failure message, Cmd.none)
                        _  ->
                            (Failure "error", Cmd.none)
        PostedEntry result ->
            case result of
                Ok _ ->
                    (model, getEntries)
                Err error ->
                    case error of
                        Http.BadBody message ->
                            (Failure message, Cmd.none)
                        _  ->
                            (Failure "error", Cmd.none)
        PostEntry ->
            (model, postEntry (Entry "-" "-" "-" "-" 1 1))

getEntries: Cmd Msg
getEntries =
    Http.request
        { method = "GET"
        , headers = [Http.header "x-api-key" "bZtakJFNc58t22fWKAfmb70ogJLOFp7F3T6Qu68D"]
        ,  url = "https://upwacz0asa.execute-api.ap-northeast-1.amazonaws.com/dev/entries"
        , body = Http.emptyBody
        , expect = Http.expectJson GotEntries Entry.listDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

postEntry : Entry -> Cmd Msg
postEntry entry =
    let
        bod =
            Entry.encode entry
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = [Http.header "x-api-key" "bZtakJFNc58t22fWKAfmb70ogJLOFp7F3T6Qu68D"]
        ,  url = "https://upwacz0asa.execute-api.ap-northeast-1.amazonaws.com/dev/entries"
        , body = bod
        , expect = Http.expectString PostedEntry
        , timeout = Nothing
        , tracker = Nothing
        }

view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text "第一回チキチキおしゃれコード選手権"]
    , viewEntries model
    , button [onClick PostEntry] [text "post"]
    ]

viewEntries model =
    case model of
        Failure error ->
            div []
            [ text error ]
        Loading ->
            text "Loading"
        Success entries ->
            div []
            ( entries |> List.sortBy (\e -> e.timestamp)  |> List.map (\e -> span [] [text e.userName]) )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
