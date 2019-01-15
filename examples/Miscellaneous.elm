module Main exposing (..)

import Example
import Http
import Json.Decode as Decode
import Script exposing (Script)
import Script.NetworkConnection as NetworkConnection exposing (NetworkConnection)
import Time


script : Script.Context -> Script Int ()
script { networkConnection } =
    Script.succeed { text = "A", number = 2 }
        |> Script.aside
            (\model ->
                Script.do
                    [ Script.printLine model.text
                    , printCurrentTime networkConnection
                    , Script.sleep 500
                    ]
            )
        |> Script.map .number
        |> Script.aside
            (\number ->
                Script.do
                    [ Script.printLine (String.fromInt number)
                    , printCurrentTime networkConnection
                    , Script.sleep 500
                    , getCurrentTime networkConnection |> Script.ignore
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
            "http://time.jsontest.com/"

        decoder =
            Decode.field "time" Decode.string
    in
    networkConnection
        |> NetworkConnection.get
            { url = url, expect = NetworkConnection.expectJson decoder }
        |> Script.mapError (always "HTTP request failed")


printCurrentTime : NetworkConnection -> Script String ()
printCurrentTime networkConnection =
    getCurrentTime networkConnection |> Script.andThen Script.printLine


main : Script.Program
main =
    Example.program script
