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

type alias EntryItem =
    { isShowDescription : Bool
    , entry : Entry
    }

type alias Model =
    { entryItems : Status (List EntryItem)
    , newEntry : EntryForm.Model
    }


type Msg
    = GotEntries (Result Http.Error (List Entry))
    | ClickEntryItem Entry
    | EntryFormMsg EntryForm.Msg

statusMap : (a -> b) -> Status a -> Status b
statusMap f s =
    case s of
        Failure m ->
            Failure m
        Loading a ->
            a |> f |> Loading
        Loaded a ->
            a |> f |> Loaded

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Loading []) EntryForm.initModel, getEntries )

toggleIsShowDescription : EntryItem -> EntryItem
toggleIsShowDescription entryItem =
    if entryItem.isShowDescription then
        { entryItem | isShowDescription = False }
    else
        { entryItem | isShowDescription = True }

equalOr : Entry -> EntryItem -> EntryItem
equalOr targetEntry entryItem =
    if entryItem.entry.track == targetEntry.track then
        toggleIsShowDescription entryItem
     else
         entryItem

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    ( { model | entryItems = entries |> List.map (EntryItem False) |> Loaded }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | entryItems = Failure message }, Cmd.none )

                        _ ->
                            ( { model | entryItems = Failure "Error" }, Cmd.none )

        ClickEntryItem entry ->
            ( { model | entryItems = model.entryItems |> statusMap ( List.map (equalOr entry) ) }, Cmd.none )

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
            , viewEntryItems model.entryItems
            ]
        ]

viewEntryDescription entryItem =
    case entryItem.isShowDescription of
        False ->
            []
        True ->
            [ Grid.col [ Col.md12 ] [ text entryItem.entry.description ] ]

viewEntryItem entryItem =
    ListGroup.li [ ListGroup.attrs [onClick (ClickEntryItem entryItem.entry),  style "cursor" "pointer" ] ]
        [ Grid.row []
            [ Grid.col [ Col.md4 ] [ text entryItem.entry.userName ]
            , Grid.col [ Col.md4 ] [ entryItem.entry.problem |> Entry.problemToString |> text ]
            , Grid.col [ Col.md4 ] [ a [ href entryItem.entry.url ] [ text entryItem.entry.url ] ]
            ]
        , Grid.row [] (viewEntryDescription entryItem)
        ]

viewEntryItems model =
    case model of
        Failure error ->
            div []
                [ text error ]

        Loading _ ->
            text "Loading"

        Loaded entryItems ->
            Grid.row []
                [ Grid.col [ Col.md12 ]
                    [ entryItems
                        |> List.sortBy (\e -> e.entry.timestamp)
                        |> List.map viewEntryItem
                        |> ListGroup.ul
                    ]
                ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
