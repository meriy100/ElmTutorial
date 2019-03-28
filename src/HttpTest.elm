module Http exposing (..)
import Html exposing (Html, text, pre)
import Http

type Model
    = Failure
    | Loading
    | Success String

type Msg
    = GotText (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:9090/output.txt"
        , expect = Http.expectString GotText
        }
    )
