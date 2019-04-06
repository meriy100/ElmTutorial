module Entry exposing (Entry, init, toProblem, problemToString, decoder, encode, listDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Problem
    = FizzBuzz
    | Fib

type alias Entry =
    { track : String
    , userName : String
    , url : String
    , description : String
    , problem : Problem
    , timestamp : Int
    }

init : Entry
init =
    Entry "" "" "" "" FizzBuzz 1

listDecoder : Decoder (List Entry)
listDecoder =
    Decode.field "Items" (Decode.list decoder)

toProblem : Int -> Problem
toProblem n =
    case n of
        1 ->
            FizzBuzz
        2 ->
            Fib
        _ ->
            FizzBuzz
fromProblem : Problem -> Int
fromProblem problem =
    case problem of
        FizzBuzz ->
            1
        Fib ->
            2
problemToString : Problem -> String
problemToString problem =
    case problem of
        FizzBuzz ->
            "FizzBuzz"
        Fib ->
            "フィボナッチ数"

problemDecoder : Decoder Problem
problemDecoder =
    Decode.map toProblem (Decode.field "problem_id" Decode.int)

decoder : Decoder Entry
decoder =
    Decode.map6 Entry
        (Decode.field "track" Decode.string)
        (Decode.field "user_name" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "description" Decode.string)
        (problemDecoder)
        (Decode.field "timestamp" Decode.int)


encode : Entry -> Value
encode entry =
    Encode.object
        [ ( "track", Encode.string entry.track )
        , ( "user_name", Encode.string entry.userName )
        , ( "url", Encode.string entry.url )
        , ( "description", Encode.string entry.description )
        , ( "problem_id", Encode.int (entry.problem |> fromProblem) )
        ]
