module Api.Endpoint exposing (..)

import Http
import Url.Builder exposing (QueryParameter)

type Endpoint
    = Endpoint String
request
  : { method : String
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    }
     -> Cmd msg

request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = [Http.header "x-api-key" "bZtakJFNc58t22fWKAfmb70ogJLOFp7F3T6Qu68D"]
        , method = config.method
        , url = unwrap config.url
        , timeout = Nothing
        , tracker = Nothing
        }

unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "https://upwacz0asa.execute-api.ap-northeast-1.amazonaws.com/dev"
        paths
        queryParams
        |> Endpoint

entries : Endpoint
entries =
    url [ "entries" ] []
