module Script.Http exposing
    ( NetworkConnection
    , get, post, request
    , Body, emptyBody, stringBody, jsonBody
    , Expect, expectString, expectJson, expectWhatever
    , Header, header
    , Error(..)
    )

{-|

@docs NetworkConnection

@docs get, post, request

@docs Body, emptyBody, stringBody, jsonBody

@docs Expect, expectString, expectJson, expectWhatever

@docs Header, header

@docs Error

-}

import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Script.Internal as Internal exposing (Script(..))


type alias NetworkConnection =
    Internal.NetworkConnection


get : NetworkConnection -> { url : String, expect : Expect a } -> Script Error a
get networkConnection { url, expect } =
    request networkConnection
        { method = "GET"
        , headers = []
        , url = url
        , body = emptyBody
        , timeout = Nothing
        , expect = expect
        }


post : NetworkConnection -> { url : String, body : Body, expect : Expect a } -> Script Error a
post networkConnection { url, body, expect } =
    request networkConnection
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , timeout = Nothing
        , expect = expect
        }


request :
    NetworkConnection
    ->
        { method : String
        , headers : List Header
        , url : String
        , body : Body
        , timeout : Maybe Duration
        , expect : Expect a
        }
    -> Script Error a
request networkConnection { method, headers, url, body, timeout, expect } =
    let
        allHeaders =
            case body of
                EmptyBody ->
                    headers

                StringBody mimeType content ->
                    Header "Content-Type" mimeType :: headers

        encodedBody =
            case body of
                EmptyBody ->
                    Encode.null

                StringBody mimeType content ->
                    Encode.string content

        encodedOptions =
            Encode.object
                [ ( "method", Encode.string method )
                , ( "headers", Encode.object (List.map headerField allHeaders) )
                , ( "body", encodedBody )
                ]

        encodedTimeout =
            case timeout of
                Just duration ->
                    Encode.float (Duration.inMilliseconds duration)

                Nothing ->
                    Encode.null

        (Expect callback) =
            expect
    in
    Invoke "http"
        (Encode.object
            [ ( "url", Encode.string url )
            , ( "options", encodedOptions )
            , ( "timeout", encodedTimeout )
            ]
        )
        (\flags ->
            Decode.oneOf
                [ Decode.map2
                    (\status responseBody ->
                        if status >= 200 && status < 300 then
                            case callback responseBody of
                                Ok value ->
                                    Succeed value

                                Err errorMessage ->
                                    Fail (BadBody errorMessage)

                        else
                            Fail (BadStatus status)
                    )
                    (Decode.field "status" Decode.int)
                    (Decode.field "body" Decode.string)
                , Decode.field "error" Decode.string
                    |> Decode.andThen
                        (\errorType ->
                            case errorType of
                                "NetworkError" ->
                                    Decode.succeed (Fail NetworkError)

                                "Timeout" ->
                                    Decode.succeed (Fail Timeout)

                                _ ->
                                    Decode.fail
                                        ("Unrecognized HTTP error type '" ++ errorType ++ "'")
                        )
                ]
        )


headerField : Header -> ( String, Value )
headerField (Header name value) =
    ( name, Encode.string value )


type Body
    = EmptyBody
    | StringBody String String


emptyBody : Body
emptyBody =
    EmptyBody


stringBody : String -> String -> Body
stringBody =
    StringBody


jsonBody : Value -> Body
jsonBody value =
    StringBody "application/json" (Encode.encode 0 value)


type Expect a
    = Expect (String -> Result String a)


expectString : Expect String
expectString =
    Expect Ok


expectJson : Decoder a -> Expect a
expectJson decoder =
    Expect (Decode.decodeString decoder >> Result.mapError Decode.errorToString)


expectWhatever : Expect ()
expectWhatever =
    Expect (always (Ok ()))


type Header
    = Header String String


header : String -> String -> Header
header =
    Header


type Error
    = Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String
