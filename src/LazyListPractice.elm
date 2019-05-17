module LazyListPractice exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import LazyList


type alias Model =
    {}


init : Model
init =
    {}


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            {}


viewCode str =
    pre [ style "padding" "10px", style "background" "#eee", style "border-radius" "10px" ]
        [ code []
            [ text str
            ]
        ]

viewExpect str =
    pre [ style "padding" "10px", style "background" "#efe", style "border-radius" "10px" ]
        [ code []
            [ text str
            ]
        ]

viewActual str =
    pre [ style "padding" "10px", style "background" "#eef", style "border-radius" "10px" ]
        [ code []
            [ text str
            ]
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ h1 [] [ text "LazyList の練習問題" ]
            , Grid.row []
                [ Grid.col [ Col.md8 ]
                    [ viewCode "LazyList.new5 1 2 3 4 5"
                    , LazyList.new5 1 2 3 4 5
                        |> LazyList.toString String.fromInt
                        |> viewExpect
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "1. LazyList.head, LazyList.tail を実装しましょう" ]
                    , p []
                        [ text "リストの先頭要素を取得する LazyList.head : LazyList a -> Maybe a を実装しましょう."
                        , br [] []
                        , text "リストの先頭以外の要素群を返す LazyList.tail : LazyList a -> LazyList a を実装しましょう" ]
                    , div []
                        [ viewCode "LazyList.new3 10 20 30 |> LazyList.head |> Maybe.withDefault -1"
                        , viewExpect "10"
                        , LazyList.new3 10 20 30 |> LazyList.head |> Maybe.withDefault -1
                            |> String.fromInt
                            |> viewActual
                        , viewCode "LazyList.Empty |> LazyList.head |> Maybe.withDefault -1"
                        , viewExpect "-1"
                        , LazyList.LazyEmpty |> LazyList.head |> Maybe.withDefault -1
                            |> String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "LazyList.new3 10 20 30 |> LazyList.tail"
                        , viewExpect "20, 30"
                        , LazyList.new3 10 20 30 |> LazyList.tail
                            |> LazyList.toString String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "LazyList.new3 10 20 30 |> LazyList.tail |> LazyList.tail |> LazyList.tail"
                        , viewExpect ""
                        , LazyList.new3 10 20 30 |> LazyList.tail |> LazyList.tail |> LazyList.tail
                            |> LazyList.toString String.fromInt
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "2. LazyList.map を実装しましょう" ]
                    , p []
                        [ text "リストの各要素を関数 f で変換する LazyList.map : (a -> b) -> LazyList a -> LazyList b を実装しましょう."
                        ]
                    , div []
                        [ viewCode "LazyList.new3 1 2 3 |> LazyList.map (\\n -> n + 30)"
                        , viewExpect "4, 5, 6 "
                        , LazyList.new3 1 2 3 |> LazyList.map (\n -> n + 30)
                            |> LazyList.toString String.fromInt
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "4. LazyList.take を実装しましょう" ]
                    , p []
                        [ text "リストの先頭から N 個の要素群を返す LazyList.map : LazyList a -> Int -> LazyList a を実装しましょう."
                        ]
                    , div []
                        [ viewCode "LazyList.new5 1 2 3 4 5 |> LazyList.take 2"
                        , viewExpect "1, 2"
                        , LazyList.new5 1 2 3 4 5 |> LazyList.take 2
                            |> LazyList.toString String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "LazyList.fill \"loop\" |> LazyList.take 2"
                        , viewExpect "loop, loop, loop, loop, loop, loop, loop, loop, loop, loop"
                        , LazyList.fill "loop" |> LazyList.take 10
                            |> LazyList.toString (\s -> s)
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "5. LazyList.upto を実装しましょう" ]
                    , p []
                        [ text "n から無限大のリストを返す LazyList.upto : Int -> LazyList Int を実装しましょう."
                        ]
                    , div []
                        [ viewCode "LazyList.upto 1 |> LazyList.take 15"
                        , viewExpect "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15"
                        , LazyList.upto 1 |> LazyList.take 15
                            |> LazyList.toString String.fromInt
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "6. LazyList.fizzBuzz を実装しましょう" ]
                    , p []
                        [ text "無限大のFizzBuzzリストを返す LazyList.fizzBuzz : LazyList FizzBuzz を実装しましょう."
                        ]
                    , div []
                        [ viewCode "LazyList.fizzBuzz |> LazyList.take 15"
                        , viewExpect "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15"
                        , LazyList.fizzBuzz |> LazyList.take 15
                            |> LazyList.toString (\s -> s)
                            |> viewActual
                        ]
                    ]
                ]
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
