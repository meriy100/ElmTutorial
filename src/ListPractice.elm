module ListPractice exposing (Model, Msg(..), init, main, update, view, viewCode, viewExpect)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MyList


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
            [ h1 [] [ text "List の練習問題" ]
            , Grid.row []
                [ Grid.col [ Col.md8 ]
                    [ viewCode "MyList.Cons 1 (MyList.Cons 2 (MyList.Cons 3 (MyList.Cons 4)))"
                    , MyList.Cons 1 (MyList.Cons 2 (MyList.Cons 3 (MyList.Cons 4 MyList.Empty)))
                        |> MyList.toString String.fromInt
                        |> viewExpect
                    ]
                , Grid.col [ Col.md8 ]
                    [ viewCode "MyList.new 1 "
                    , MyList.new 1
                        |> MyList.toString String.fromInt
                        |> viewExpect
                    ]
                , Grid.col [ Col.md8 ]
                    [ viewCode "MyList.new2 1 2"
                    , MyList.new2 1 2
                        |> MyList.toString String.fromInt
                        |> viewExpect
                    ]
                , Grid.col [ Col.md8 ]
                    [ viewCode "MyList.new3 1 2 3"
                    , MyList.new3 1 2 3
                        |> MyList.toString String.fromInt
                        |> viewExpect
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "1. MyList.head, MyList.tail を実装しましょう" ]
                    , p []
                        [ text "リストの先頭要素を取得する MyList.head : MyList a -> Maybe a を実装しましょう."
                        , br [] []
                        , text "リストの先頭以外の要素群を返す MyList.tail : MyList a -> MyList a を実装しましょう" ]
                    , div []
                        [ viewCode "MyList.new3 1 2 3 |> MyList.head |> Maybe.withDefault -1"
                        , viewExpect "1"
                        , MyList.new3 1 2 3 |> MyList.head |> Maybe.withDefault -1
                            |> String.fromInt
                            |> viewActual
                        , viewCode "MyList.Empty |> MyList.head |> Maybe.withDefault -1"
                        , viewExpect "-1"
                        , MyList.Empty |> MyList.head |> Maybe.withDefault -1
                            |> String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "MyList.new3 1 2 3 |> MyList.tail"
                        , viewExpect "2, 3"
                        , MyList.new3 1 2 3 |> MyList.tail
                            |> MyList.toString String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "MyList.new3 1 2 3 |> MyList.tail |> MyList.tail |> MyList.tail"
                        , viewExpect ""
                        , MyList.new3 1 2 3 |> MyList.tail |> MyList.tail |> MyList.tail
                            |> MyList.toString String.fromInt
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "2. MyList.map を実装しましょう" ]
                    , p []
                        [ text "リストの各要素を関数 f で変換する MyList.map : (a -> b) -> MyList a -> MyList b を実装しましょう."
                        ]
                    , div []
                        [ viewCode "MyList.new3 1 2 3 |> MyList.map (\\n -> n + 3)"
                        , viewExpect "4, 5, 6 "
                        , MyList.new3 1 2 3 |> MyList.map (\n -> n + 3)
                            |> MyList.toString String.fromInt
                            |> viewActual
                        ]
                    ]
                , Grid.col [ Col.md8 ]
                    [ h4 [] [ text "2. MyList.take を実装しましょう" ]
                    , p []
                        [ text "リストの先頭から N 個の要素群を返す MyList.map : MyList a -> Int -> MyList a を実装しましょう."
                        ]
                    , div []
                        [ viewCode "MyList.new5 1 2 3 4 5 |> MyList.take 2"
                        , viewExpect "1, 2"
                        , MyList.new5 1 2 3 4 5 |> MyList.take 2
                            |> MyList.toString String.fromInt
                            |> viewActual
                        ]
                    , div []
                        [ viewCode "MyList.fill \"loop\" |> MyList.take 2"
                        , viewExpect "loop, loop, loop, loop, loop, loop, loop, loop, loop, loop"
--                        , MyList.fill "loop" |> MyList.take 2
--                            |> MyList.toString (\s -> s)
--                            |> viewActual
                        ]
                    ]
                ]
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
