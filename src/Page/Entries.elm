module Page.Entries exposing (Model, Msg(..), Status(..), getEntries, init, postEntry, subscriptions, update, view, viewEntries)

import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Entry as Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type Status a
    = Failure String
    | Loading a
    | Loaded a


type alias Model =
    { entries : Status (List Entry)
    , newEntry : Status Entry
    }


type Msg
    = GotEntries (Result Http.Error (List Entry))
    | PostedEntry (Result Http.Error String)
    | InputUserName String
    | InputUrl String
    | InputDescription String
    | SubmitEntry


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Loading []) (Loaded Entry.init), getEntries )


updateNewEntry : Msg -> Status Entry -> Entry
updateNewEntry msg model =
    let
        newEntry =
            case model of
                Loaded a ->
                    a

                Loading a ->
                    a

                Failure _ ->
                    Entry.init
    in
    case msg of
        InputUserName userName ->
            { newEntry | userName = userName }

        InputUrl url ->
            { newEntry | url = url }

        InputDescription description ->
            { newEntry | description = description }

        _ ->
            newEntry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    ( { model | entries = Loaded entries }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | entries = Failure message }, Cmd.none )

                        _ ->
                            ( { model | entries = Failure "Error" }, Cmd.none )

        PostedEntry result ->
            case result of
                Ok _ ->
                    ( { model | newEntry = Loaded Entry.init }, getEntries )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | newEntry = Failure message }, Cmd.none )
                        _ ->
                            ( { model | newEntry = Failure "Error" }, Cmd.none )

        InputUserName _ ->
            ( { model | newEntry = Loaded (updateNewEntry msg model.newEntry) }, Cmd.none )

        InputUrl _ ->
            ( { model | newEntry = Loaded (updateNewEntry msg model.newEntry) }, Cmd.none )

        InputDescription _ ->
            ( { model | newEntry = Loaded (updateNewEntry msg model.newEntry) }, Cmd.none )

        SubmitEntry ->
            case model.newEntry of
                Loaded newEntry ->
                    ( model, postEntry newEntry )

                Loading _ ->
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
            , viewEntryForm model.newEntry
            , viewEntries model.entries
            ]
        ]


viewEntryForm model =
    case model of
        Failure error ->
            div []
                [ text error ]

        Loading _ ->
            text "Loading"

        Loaded entry ->
            Form.form []
                [ Form.group []
                    [ Form.label [ for "user_name" ] [ text "投稿者名" ]
                    , Input.text [ Input.id "user_name", Input.value entry.userName, Input.onInput InputUserName ]
                    ]
                , Form.group []
                    [ Form.label [ for "url" ] [ text "Git URL" ]
                    , Input.text [ Input.id "url", Input.value entry.url, Input.onInput InputUrl ]
                    ]
                , Form.group []
                    [ Form.label [ for "description" ] [ text "簡単な解説" ]
                    , Textarea.textarea
                        [ Textarea.id "description"
                        , Textarea.value entry.description
                        , Textarea.onInput InputDescription
                        ]
                    ]
                , Button.button [ Button.primary, Button.onClick SubmitEntry ] [ text "投稿" ]
                ]


viewEntries model =
    case model of
        Failure error ->
            div []
                [ text error ]

        Loading _ ->
            text "Loading"

        Loaded entries ->
            div []
                (entries |> List.sortBy (\e -> e.timestamp) |> List.map (\e -> span [] [ text e.userName ]))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none