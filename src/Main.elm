module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Entry as Entry exposing (Entry)
import Api.Endpoint as Endpoint

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

requestError error =
                    case error of
                        Http.BadBody message ->
                            (Failure message, Cmd.none)
                        _  ->
                            (Failure "error", Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    (Success entries, Cmd.none)
                Err error ->
                    requestError error
        PostedEntry result ->
            case result of
                Ok _ ->
                    (model, getEntries)
                Err error ->
                    requestError error
        PostEntry ->
            (model, postEntry (Entry "-" "-" "-" "-" 1 1))

getEntries: Cmd Msg
getEntries =
    Endpoint.request
        { method = "GET"
        , url = Endpoint.entries
        , body = Http.emptyBody
        , expect = Http.expectJson GotEntries Entry.listDecoder
        }

postEntry : Entry -> Cmd Msg
postEntry entry =
    let
        bod =
            Entry.encode entry
                |> Http.jsonBody
    in
    Endpoint.request
        { method = "POST"
        , url = Endpoint.entries
        , body = bod
        , expect = Http.expectString PostedEntry
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
