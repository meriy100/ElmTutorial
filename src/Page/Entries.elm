module Page.Entries exposing (..)

import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Entry as Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Page.Entries.EntryForm as EntryForm


type Status a
    = Failure String
    | Loading a
    | Loaded a


type alias Model =
    { entries : Status (List Entry)
    , newEntry : EntryForm.Model
    , showEntry : Maybe Entry
    }


type Msg
    = GotEntries (Result Http.Error (List Entry))
    | SelectEntry Entry
    | EntryFormMsg EntryForm.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Loading []) EntryForm.initModel Nothing, getEntries )


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

        SelectEntry entry ->
            ( { model | showEntry = Just entry }, Cmd.none )

        EntryFormMsg (EntryForm.PostedEntry result) ->
            let
                ( newEntry, _ ) =
                    EntryForm.update (EntryForm.PostedEntry result) model.newEntry
            in
            ( { model | newEntry = newEntry }, getEntries )

        EntryFormMsg entryFormMsg ->
            let
                ( newEntry, cmd ) =
                    EntryForm.update entryFormMsg model.newEntry
            in
            ( { model | newEntry = newEntry }, Cmd.map EntryFormMsg cmd )


getEntries : Cmd Msg
getEntries =
    Endpoint.request
        { method = "GET"
        , url = Endpoint.entries
        , body = Http.emptyBody
        , expect = Http.expectJson GotEntries Entry.listDecoder
        }


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ h1 [] [ text "第一回チキチキおしゃれコード選手権" ]
            , EntryForm.viewEntryForm model.newEntry |> Html.map EntryFormMsg
            , viewEntries model.entries
            ]
        ]

viewEntryItem entry =
    ListGroup.li []
        [ Grid.row []
            [ Grid.col [ Col.md4 ] [ text entry.userName ]
            , Grid.col [ Col.md4 ] [ entry.problem |> Entry.problemToString |> text ]
            , Grid.col [ Col.md4 ] [ a [ href entry.url ] [ text entry.url ] ]
            ]
        ]

viewEntries model =
    case model of
        Failure error ->
            div []
                [ text error ]

        Loading _ ->
            text "Loading"

        Loaded entries ->
            Grid.row []
                [ Grid.col [ Col.md12 ]
                    [ entries
                        |> List.sortBy (\e -> e.timestamp)
                        |> List.map viewEntryItem
                        |> ListGroup.ul
                    ]
                ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
