module Form exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex exposing (..)


type alias Model =
    { name : String
    , age : Maybe Int
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" Nothing  "" ""


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }
        Age age ->
            { model | age = String.toInt age }
        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

validate : (Model -> Bool) -> String -> Model ->  Maybe String
validate f message model =
    if f model then
        Just message
    else
        Nothing

passwordMatchValidate : Model -> Maybe String
passwordMatchValidate model =
    validate (\m -> m.password /= m.passwordAgain) "Passwords do not match!" model
passwordLengthValidate model =
    validate (\m -> String.length m.password < 8) "Passwords is not over 8 chars" model

isComplexly : String -> Bool
isComplexly str =
    not(Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[0-9]") str &&
        Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[A-Z]") str &&
        Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[a-z]") str)

passwordComplexlyValidate model =
    validate (\m -> m.password |> isComplexly) "Passwords is not lower case and upper case and numeric" model

viewValidation : (Model -> Maybe String) -> Model -> Html msg
viewValidation validator model =
    case validator model of
        Just message ->
            div [ style "color" "Red" ] [ message |> text ]
        Nothing ->
            div [ style "color" "Green" ] [ "OK" |> text ]


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "number" "Age" ((Maybe.withDefault 0 model.age) |> String.fromInt ) Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Password" model.passwordAgain PasswordAgain
        , viewValidation passwordMatchValidate model
        , viewValidation passwordLengthValidate model
        , viewValidation passwordComplexlyValidate model
        ]


main =
    Browser.sandbox { init = init, view = view, update = update }
