module Entry exposing (Entry, decoder, encode, listDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Entry =
    { track : String
    , userName : String
    , url : String
    , description : String
    , problemId : Int
    , timestamp : Int
    }


listDecoder : Decoder (List Entry)
listDecoder =
    Decode.field "Items" (Decode.list decoder)


decoder : Decoder Entry
decoder =
    Decode.map6 Entry (Decode.field "track" Decode.string) (Decode.field "user_name" Decode.string) (Decode.field "url" Decode.string) (Decode.field "description" Decode.string) (Decode.field "problem_id" Decode.int) (Decode.field "timestamp" Decode.int)


encode : Entry -> Value
encode entry =
    Encode.object
        [ ( "track", Encode.string entry.track )
        , ( "user_name", Encode.string entry.userName )
        , ( "url", Encode.string entry.url )
        , ( "description", Encode.string entry.description )
        , ( "problem_id", Encode.int entry.problemId )
        ]
