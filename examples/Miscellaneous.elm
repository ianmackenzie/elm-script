module Miscellaneous exposing (main)

import Duration
import Example
import Json.Decode as Decode
import Script exposing (Script)
import Script.Http as Http exposing (NetworkConnection)
import Time


script : Script.Init -> Script Int ()
script { networkConnection } =
    Script.succeed { text = "A", number = 2 }
        |> Script.aside
            (\model ->
                Script.do
                    [ Script.printLine model.text
                    , printCurrentTime networkConnection
                    , Script.sleep (Duration.seconds 0.5)
                    ]
            )
        |> Script.map .number
        |> Script.aside
            (\number ->
                Script.do
                    [ Script.printLine (String.fromInt number)
                    , printCurrentTime networkConnection
                    , Script.sleep (Duration.seconds 0.5)
                    , getCurrentTime networkConnection |> Script.ignoreResult
                    ]
            )
        |> Script.andThen
            (\number ->
                if number > 2 then
                    Script.succeed ()

                else
                    Script.fail "Ugh, number is too small"
            )
        |> Script.onError (Example.handleError identity)


getCurrentTime : NetworkConnection -> Script String String
getCurrentTime networkConnection =
    let
        url =
            "http://worldtimeapi.org/api/ip"

        decoder =
            Decode.field "datetime" Decode.string
    in
    Http.get networkConnection
        { url = url
        , expect = Http.expectJson decoder
        }
        |> Script.mapError (always "HTTP request failed")


printCurrentTime : NetworkConnection -> Script String ()
printCurrentTime networkConnection =
    getCurrentTime networkConnection |> Script.andThen Script.printLine


main : Script.Program
main =
    Example.program script
