module Page.Entries exposing (Model, Msg(..), Status(..), getEntries, init, postEntry, subscriptions, update, view, viewEntries)

import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Entry as Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type Status a
    = Failure String
    | Loading
    | Success a


type alias Model =
    { entries : Status (List Entry)
    , newEntry : Status Entry
    }


type Msg
    = GotEntries (Result Http.Error (List Entry))
    | PostedEntry (Result Http.Error String)
    | PostEntry


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading (Success Entry.init), getEntries )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    ( { model | entries = Success entries }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | entries = Failure message }, Cmd.none )

                        _ ->
                            ( { model | entries = Failure "Error" }, Cmd.none )

        PostedEntry result ->
            case result of
                Ok _ ->
                    ( model, getEntries )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | newEntry = Failure message }, Cmd.none )

                        _ ->
                            ( { model | newEntry = Failure "Error" }, Cmd.none )

        PostEntry ->
            case model.newEntry of
                Success newEntry ->
                    ( model, postEntry newEntry )
                Loading ->
                    ( model, Cmd.none )
                Failure _ ->
                    ( model, Cmd.none )


getEntries : Cmd Msg
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
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ h1 [] [ text "第一回チキチキおしゃれコード選手権" ]
            , viewEntries model.entries
            , Button.button [ Button.primary, Button.onClick PostEntry ] [ text "post" ]
            ]
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
                (entries |> List.sortBy (\e -> e.timestamp) |> List.map (\e -> span [] [ text e.userName ]))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
