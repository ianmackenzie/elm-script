module Script.NetworkConnection exposing
    ( Expect
    , NetworkConnection
    , expectJson
    , expectString
    , expectStringResponse
    , get
    , post
    )

import Duration exposing (Duration)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Script.Internal as Internal


type alias NetworkConnection =
    Internal.NetworkConnection


type Expect x a
    = Expect (Http.Expect (Internal.Script x a))


toHttpExpect : Expect x a -> Http.Expect (Internal.Script x a)
toHttpExpect (Expect httpExpect) =
    httpExpect


resultToScript : Result x a -> Internal.Script x a
resultToScript result =
    case result of
        Ok value ->
            Internal.Succeed value

        Err error ->
            Internal.Fail error


expectString : Expect Http.Error String
expectString =
    Expect (Http.expectString resultToScript)


expectJson : Decoder a -> Expect Http.Error a
expectJson decoder =
    Expect (Http.expectJson resultToScript decoder)


expectStringResponse : (Http.Response String -> Result x a) -> Expect x a
expectStringResponse handler =
    Expect (Http.expectStringResponse resultToScript handler)


get : { url : String, expect : Expect x a } -> NetworkConnection -> Internal.Script x a
get { url, expect } networkConnection =
    Internal.Do <|
        Http.get { url = url, expect = toHttpExpect expect }


post : { url : String, body : Http.Body, expect : Expect x a } -> NetworkConnection -> Internal.Script x a
post { url, body, expect } networkConnection =
    Internal.Do <|
        Http.post { url = url, body = body, expect = toHttpExpect expect }


request :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Expect x a
    , timeout : Maybe Duration
    }
    -> Internal.Script x a
request { method, headers, url, body, expect, timeout } =
    Internal.Do <|
        Http.request
            { method = method
            , headers = headers
            , url = url
            , body = body
            , expect = toHttpExpect expect
            , timeout = Maybe.map Duration.inMilliseconds timeout
            , tracker = Nothing
            }
