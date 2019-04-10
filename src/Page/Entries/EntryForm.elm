module Page.Entries.EntryForm exposing (Field(..), Model, Msg(..), Status(..), ValidateError, ValidateFilter, dangerWith, filterDescription, filterUrl, filterUserName, getEntry, initModel, invalidFeedback, isLoading, modelValidate, modelValidator, postEntry, update, view)

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


type Msg
    = PostedEntry (Result Http.Error String)
    | InputUserName String
    | InputUrl String
    | InputDescription String
    | ChangeProblem String
    | SubmitEntry


type Field
    = UserName
    | Url
    | Description


type alias ValidateError =
    ( Field, String )


type alias ValidateFilter =
    ValidateError -> Bool

replace : Entry -> Model -> Model
replace e model =
    case model of
        Failure m _ ->
            Failure m e

        Loaded _ ->
            Loaded e

        Loading _ ->
            Loaded e

getValidateMessage : ValidateError -> String
getValidateMessage (_, m) =
    m

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


modelValidator : Validator ValidateError Model
modelValidator =
    Validate.all
        [ ifBlank (.userName << getEntry) ( UserName, "Please enter a name." )
        , ifBlank (.url << getEntry) ( Url, "Please enter a url." )
        , ifBlank (.description << getEntry) ( Description, "Please enter a description." )
        ]


modelValidate : ValidateFilter -> Model -> Result (List ValidateError) (Valid Model)
modelValidate f model =
    validate modelValidator model |> Result.mapError (\xs -> List.filter f xs)


filterUserName : ValidateFilter
filterUserName ( f, _ ) =
    case f of
        UserName ->
            True

        _ ->
            False


filterUrl : ValidateFilter
filterUrl ( f, _ ) =
    case f of
        Url ->
            True

        _ ->
            False


filterDescription : ValidateFilter
filterDescription ( f, _ ) =
    case f of
        Description ->
            True

        _ ->
            False


dangerWith : (Model -> Result (List ValidateError) (Valid Model)) -> Model -> a -> List a -> List a
dangerWith v model danger xs =
    case v model of
        Ok _ ->
            xs

        Err ys ->
            case ys of
                [] ->
                    xs
                _ ->
                    case model of
                        Failure _ _ ->
                            danger :: xs

                        _ ->
                            xs


invalidFeedback : (Model -> Result (List ValidateError) (Valid Model)) -> Model -> Html Msg
invalidFeedback v model =
    case v model of
        Ok _ ->
            text ""

        Err messages ->
            case model of
                Failure _ _ ->
                    Form.invalidFeedback [] [ text (messages |> List.head |> Maybe.withDefault (UserName, "") |> getValidateMessage) ]

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
            ( replace { newEntry | userName = userName } model, Cmd.none )

        InputUrl url ->
            ( replace { newEntry | url = url } model, Cmd.none )

        InputDescription description ->
            ( replace { newEntry | description = description } model, Cmd.none )

        ChangeProblem problemId ->
            ( replace { newEntry | problem = problemId |> String.toInt |> Maybe.withDefault 1 |> Entry.toProblem } model, Cmd.none )

        SubmitEntry ->
            case model of
                Loading _ ->
                    ( model, Cmd.none )

                Loaded a ->
                    case validate modelValidator model of
                        Ok _ ->
                            ( Loading a, postEntry a )
                        Err _ ->
                            ( Failure "invalid" a, Cmd.none )

                Failure _ a ->
                    case validate modelValidator model of
                        Ok _ ->
                            ( Loading a, postEntry a )
                        Err _ ->
                            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        entry =
            getEntry model
    in
    Form.form []
        [ Form.group []
            [ Form.label [ for "user_name" ] [ text "投稿者名" ]
            , [ Input.id "user_name", Input.value entry.userName, Input.onInput InputUserName ]
                |> dangerWith (modelValidate filterUserName) model Input.danger
                |> Input.text
            , invalidFeedback (modelValidate filterUserName) model
            ]
        , Form.group []
            [ Form.label [ for "problem" ] [ text "課題名" ]
            , Select.select
                [ Select.id "problem"
                , Select.onChange ChangeProblem
                ]
                [ Select.item [ value "1" ] [ text "FizzBuzz" ]
                , Select.item [ value "2" ] [ text "最大公約数" ]
                ]
            ]
        , Form.group []
            [ Form.label [ for "url" ] [ text "Git URL" ]
            , [ Input.id "url", Input.value entry.url, Input.onInput InputUrl ]
                |> dangerWith (modelValidate filterUrl) model Input.danger
                |> Input.text
            , invalidFeedback (modelValidate filterUrl) model
            ]
        , Form.group []
            [ Form.label [ for "description" ] [ text "簡単な解説" ]
            , [ Textarea.id "description", Textarea.value entry.description, Textarea.onInput InputDescription ]
                |> dangerWith (modelValidate filterDescription) model Textarea.danger
                |> Textarea.textarea
            , invalidFeedback (modelValidate filterDescription) model
            ]
        , Button.button [ Button.primary, Button.onClick SubmitEntry, Button.disabled (isLoading model) ] [ text "投稿" ]
        ]
