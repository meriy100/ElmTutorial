module Page.Entries.EntryForm exposing (Model, Msg(..), Status(..), initModel, postEntry, update, view)

import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Entry as Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type Status a
    = Failure String
    | Loading a
    | Loaded a


type alias Model =
    Status Entry


type Msg
    = PostedEntry (Result Http.Error String)
    | InputUserName String
    | InputUrl String
    | InputDescription String
    | ChangeProblem String
    | SubmitEntry


initModel : Model
initModel =
    Loaded Entry.init


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


update : Msg -> Status Entry -> ( Model, Cmd Msg )
update msg model =
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
        PostedEntry result ->
            case result of
                Ok _ ->
                    ( initModel, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( Failure message, Cmd.none )

                        _ ->
                            ( Failure "Error", Cmd.none )

        InputUserName userName ->
            ( Loaded { newEntry | userName = userName }, Cmd.none )

        InputUrl url ->
            ( Loaded { newEntry | url = url }, Cmd.none )

        InputDescription description ->
            ( Loaded { newEntry | description = description }, Cmd.none )

        ChangeProblem problemId ->
            ( Loaded { newEntry | problem = problemId |> String.toInt |> Maybe.withDefault 1 |> Entry.toProblem }, Cmd.none )

        SubmitEntry ->
            case model of
                Loaded a ->
                    ( model, postEntry a )

                Loading _ ->
                    ( model, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
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
                    [ Form.label [ for "problem" ] [ text "課題名" ]
                    , Select.select
                        [ Select.id "problem"
                        , Select.onChange ChangeProblem
                        ]
                        [ Select.item [ value "1" ] [ text "FizzBuzz" ]
                        , Select.item [ value "2" ] [ text "フィボナッチ数" ]
                        ]
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
