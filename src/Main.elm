module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (Value)

type alias Entry =
    { track : String
    , userName : String
    , url : String
    , description : String
    , problemId: Int
    , timestamp : Int
    }

type Model
    = Failure String
    | Loading
    | Success (List Entry)


type Msg
    = GotEntries (Result Http.Error (List Entry))


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


getEntries: Cmd Msg
getEntries =
    Http.request
        { method = "GET"
        , headers = [Http.header "x-api-key" "bZtakJFNc58t22fWKAfmb70ogJLOFp7F3T6Qu68D"]
        ,  url = "https://upwacz0asa.execute-api.ap-northeast-1.amazonaws.com/dev/entries"
        , body = Http.emptyBody
        , expect = Http.expectJson GotEntries entriesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

entriesDecoder: Decoder (List Entry)
entriesDecoder =
    Decode.field "Items" (Decode.list entryDecoder)

entryDecoder: Decoder Entry
entryDecoder =
    Decode.map6 Entry (Decode.field "track" Decode.string) (Decode.field "user_name" Decode.string) (Decode.field "url" Decode.string) (Decode.field "description" Decode.string) (Decode.field "problem_id" Decode.int) (Decode.field "timestamp" Decode.int)

view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text "第一回チキチキおしゃれコード選手権"]
    , viewEntries model
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
            ( entries |> List.map (\e -> span [] [text e.userName]) )

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
