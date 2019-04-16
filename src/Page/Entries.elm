module Page.Entries exposing (EntryItem(..), Model, Msg(..), Status(..), equalOr, fetchEntries, getEntry, init, statusMap, subscriptions, toggleVisible, update, view, viewEntryDescription, viewEntryItem, viewEntryItems)

import Api.Endpoint as Endpoint
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Entry as Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Page.Entries.EntryForm as EntryForm
import Regex


type Status a
    = Failure String
    | Loading a
    | Loaded a


type EntryItem
    = Show Entry
    | Hide Entry


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
    ( Model (Loading []) EntryForm.initModel, fetchEntries )


toggleVisible : EntryItem -> EntryItem
toggleVisible entry =
    case entry of
        Show e ->
            Hide e

        Hide e ->
            Show e


getEntry : EntryItem -> Entry
getEntry entryItem =
    case entryItem of
        Show e ->
            e

        Hide e ->
            e


equalOr : Entry -> EntryItem -> EntryItem
equalOr targetEntry entryItem =
    if .track (getEntry entryItem) == targetEntry.track then
        toggleVisible entryItem

    else
        entryItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEntries result ->
            case result of
                Ok entries ->
                    ( { model | entryItems = entries |> List.map Hide |> Loaded }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | entryItems = Failure message }, Cmd.none )

                        _ ->
                            ( { model | entryItems = Failure "Error" }, Cmd.none )

        ClickEntryItem entry ->
            ( { model | entryItems = model.entryItems |> statusMap (List.map (equalOr entry)) }, Cmd.none )

        EntryFormMsg (EntryForm.PostedEntry result) ->
            let
                ( newEntry, _ ) =
                    EntryForm.update (EntryForm.PostedEntry result) model.newEntry
            in
            ( { model | newEntry = newEntry }, fetchEntries )

        EntryFormMsg entryFormMsg ->
            let
                ( newEntry, cmd ) =
                    EntryForm.update entryFormMsg model.newEntry
            in
            ( { model | newEntry = newEntry }, Cmd.map EntryFormMsg cmd )


fetchEntries : Cmd Msg
fetchEntries =
    Endpoint.request
        { method = "GET"
        , url = Endpoint.entries
        , body = Http.emptyBody
        , expect = Http.expectJson GotEntries Entry.listDecoder
        }

splitNl : String -> List String
splitNl str =
    Regex.split (Maybe.withDefault Regex.never <| Regex.fromString "\n") str

nl2br : List String -> List (Html Msg)
nl2br strings =
    case strings of
        (s::ss) ->
           (text s :: (br [] [] :: nl2br ss))
        [] ->
            []

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ h1 [] [ text "第一回チキチキおしゃれコード選手権" ]
            , div []
                [ a [ href "https://startuptechnology.esa.io/posts/10111" ] [ text "概要"] ]
            , EntryForm.view model.newEntry |> Html.map EntryFormMsg
            , viewEntryItems model.entryItems
            ]
        ]


viewEntryDescription : EntryItem -> List (Grid.Column Msg)
viewEntryDescription entryItem =
    case entryItem of
        Hide _ ->
            []

        Show entry ->
            [ Grid.col [ Col.md12 ] (entry.description |> splitNl |> nl2br) ]


viewEntryItem : EntryItem -> ListGroup.Item Msg
viewEntryItem entryItem =
    ListGroup.li [ ListGroup.attrs [ onClick (entryItem |> getEntry |> ClickEntryItem), style "cursor" "pointer" ] ]
        [ Grid.row []
            [ Grid.col [ Col.md4 ]
                [ h5 [] [ entryItem |> getEntry |> .userName |> text ] ]
            , Grid.col [ Col.md4 ]
                [ h6 [] [ entryItem |> getEntry |> .problem |> Entry.problemToString |> text ] ]
            , Grid.col [ Col.md4 ] [ a [ entryItem |> getEntry |> .url |> href ] [ text "Gist" ] ]
            ]
        , Grid.row [] (viewEntryDescription entryItem)
        ]


viewEntryItems : Status (List EntryItem) -> Html Msg
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
                        |> List.sortBy (\e -> e |> getEntry |> .timestamp |> Maybe.withDefault 0)
                        |> List.map viewEntryItem
                        |> ListGroup.ul
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
