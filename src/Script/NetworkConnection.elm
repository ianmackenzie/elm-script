module Script.NetworkConnection exposing
    ( Expect
    , NetworkConnection
    , expectJson
    , expectString
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


type Expect a
    = Expect (Http.Expect (Internal.Script Http.Error a))


toHttpExpect : Expect a -> Http.Expect (Internal.Script Http.Error a)
toHttpExpect (Expect httpExpect) =
    httpExpect


resultToScript : Result Http.Error a -> Internal.Script Http.Error a
resultToScript result =
    case result of
        Ok value ->
            Internal.Succeed value

        Err error ->
            Internal.Fail error


expectString : Expect String
expectString =
    Expect (Http.expectString resultToScript)


expectJson : Decoder a -> Expect a
expectJson decoder =
    Expect (Http.expectJson resultToScript decoder)


get : { url : String, expect : Expect a } -> NetworkConnection -> Internal.Script Http.Error a
get { url, expect } networkConnection =
    Internal.Do <|
        Http.get { url = url, expect = toHttpExpect expect }


post : { url : String, body : Http.Body, expect : Expect a } -> NetworkConnection -> Internal.Script Http.Error a
post { url, body, expect } networkConnection =
    Internal.Do <|
        Http.post { url = url, body = body, expect = toHttpExpect expect }


request :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Expect a
    , timeout : Maybe Duration
    }
    -> Internal.Script Http.Error a
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
