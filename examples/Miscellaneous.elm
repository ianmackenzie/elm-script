port module Main exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.NetworkConnection as NetworkConnection exposing (NetworkConnection)
import Time exposing (Time)


script : Script.Context -> Script Int ()
script { networkConnection } =
    Script.succeed { text = "A", number = 2 }
        |> Script.aside
            (\model ->
                Script.do
                    [ Script.printLine model.text
                    , printCurrentTime networkConnection
                    , Script.sleep (0.5 * Time.second)
                    ]
            )
        |> Script.map .number
        |> Script.aside
            (\number ->
                Script.do
                    [ Script.printLine (toString number)
                    , printCurrentTime networkConnection
                    , Script.sleep (0.5 * Time.second)
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
        |> Script.onError handleError


getCurrentTime : NetworkConnection -> Script String String
getCurrentTime networkConnection =
    let
        url =
            "http://time.jsontest.com/"

        decoder =
            Decode.field "time" Decode.string
    in
    networkConnection
        |> NetworkConnection.sendRequest (Http.get url decoder)
        |> Script.mapError (always "HTTP request failed")


printCurrentTime : NetworkConnection -> Script String ()
printCurrentTime networkConnection =
    getCurrentTime networkConnection |> Script.andThen Script.printLine


handleError : String -> Script Int a
handleError message =
    Script.printLine ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
