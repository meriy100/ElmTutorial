module Page.Entries.EntryForm exposing (..)

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
import Validate exposing (..)


type Status a
    = Failure String a
    | Loading a
    | Loaded a


type alias Model =
    Status Entry

type Field
    = UserName
    | Url
    | Description

type Msg
    = PostedEntry (Result Http.Error String)
    | InputUserName String
    | InputUrl String
    | InputDescription String
    | ChangeProblem String
    | SubmitEntry

isLoading : Model -> Bool
isLoading model =
    case model of
        Loading _ ->
            True
        _ ->
            False

getEntry : Model -> Entry
getEntry model =
    case model of
        Loaded a ->
            a

        Loading a ->
            a

        Failure _ a ->
            a

modelValidator : Validator (Field, String) Model
modelValidator =
    Validate.all
        [ ifBlank (.userName << getEntry) (UserName, "Please enter a name.")
        , ifBlank (.url << getEntry) (Url, "Please enter a url.")
        , ifBlank (.description << getEntry) (Description, "Please enter a description.")
        ]

userNameValidator : Validator String Model
userNameValidator =
    Validate.all
        [ ifBlank (.userName << getEntry) "Please enter a name."
        ]

urlValidator : Validator String Model
urlValidator =
    Validate.all
        [ ifBlank (.url << getEntry) "Please enter a url."
        ]

descriptionValidator : Validator String Model
descriptionValidator =
    Validate.all
        [ ifBlank (.description << getEntry) "Please enter a description."
        ]

dangerWith : (Validator String Model) -> Model -> a -> List a -> List a
dangerWith validator model danger xs =
    case validate validator model of
        Ok _ ->
            xs
        Err _ ->
            case model of
                Failure _ _ ->
                    (danger :: xs)
                _ ->
                    xs

invalidFeedback : (Validator String Model) -> Model -> Html Msg
invalidFeedback validator model =
    case validate validator model of
        Ok _ ->
            text ""
        Err messages ->
            case model of
                Failure _ _ ->
                    Form.invalidFeedback [] [ text (messages |> List.head |> Maybe.withDefault "")]
                _ ->
                    text ""

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
            getEntry model
    in
    case msg of
        PostedEntry result ->
            case result of
                Ok _ ->
                    ( initModel, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( Failure message newEntry, Cmd.none )

                        _ ->
                            ( Failure "Error" newEntry, Cmd.none )

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
                Loading _ ->
                    ( model, Cmd.none )

                Loaded a ->
                    ( Loading a, postEntry a )

                Failure _ a ->
                    ( Loading a, postEntry a )

view : Model -> Html Msg
view model =
    let
        entry = getEntry model
    in
    Form.form []
        [ Form.group []
            [ Form.label [ for "user_name" ] [ text "投稿者名" ]
            , [ Input.id "user_name", Input.value entry.userName, Input.onInput InputUserName ]
            |> dangerWith userNameValidator model Input.danger
            |> Input.text
            , invalidFeedback userNameValidator model
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
            , [ Input.id "url", Input.value entry.url, Input.onInput InputUrl ]
            |> dangerWith urlValidator model Input.danger
            |> Input.text
            , invalidFeedback userNameValidator model
            ]
        , Form.group []
            [ Form.label [ for "description" ] [ text "簡単な解説" ]
            , [ Textarea.id "description" , Textarea.value entry.description , Textarea.onInput InputDescription ]
            |> dangerWith descriptionValidator model Textarea.danger
            |> Textarea.textarea
            , invalidFeedback descriptionValidator model
            ]
        , Button.button [ Button.primary, Button.onClick SubmitEntry, Button.disabled (isLoading model) ] [ text "投稿" ]
        ]
