module Page.Books exposing (..)
import Html exposing (..)

viewBooks : String -> Html msg
viewBooks urlString =
    li [] [ text("this is Book" ++ urlString) ]
