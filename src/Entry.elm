module Entry exposing (Entry, init, toProblem, problemToString, decoder, encode, listDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Problem
    = FizzBuzz
    | GCD

type alias Entry =
    { track : Maybe String
    , userName : String
    , url : String
    , description : String
    , problem : Problem
    , timestamp : Maybe Int
    }

init : Entry
init =
    Entry Nothing "" "" "" FizzBuzz Nothing

listDecoder : Decoder (List Entry)
listDecoder =
    Decode.field "Items" (Decode.list decoder)

toProblem : Int -> Problem
toProblem n =
    case n of
        1 ->
            FizzBuzz
        2 ->
            GCD
        _ ->
            FizzBuzz
fromProblem : Problem -> Int
fromProblem problem =
    case problem of
        FizzBuzz ->
            1
        GCD ->
            2
problemToString : Problem -> String
problemToString problem =
    case problem of
        FizzBuzz ->
            "FizzBuzz"
        GCD ->
            "最大公約数"

problemDecoder : Decoder Problem
problemDecoder =
    Decode.map toProblem (Decode.field "problem_id" Decode.int)

decoder : Decoder Entry
decoder =
    Decode.map6 Entry
        (Decode.field "track" (Decode.maybe Decode.string))
        (Decode.field "user_name" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "description" Decode.string)
        (problemDecoder)
        (Decode.field "timestamp" (Decode.maybe Decode.int))


encode : Entry -> Value
encode entry =
    Encode.object
        [  ( "user_name", Encode.string entry.userName )
        , ( "url", Encode.string entry.url )
        , ( "description", Encode.string entry.description )
        , ( "problem_id", Encode.int (entry.problem |> fromProblem) )
        ]
