port module Main exposing (..)

import Kintail.Script as Script exposing (Script)
import Time exposing (Time)
import Json.Encode exposing (Value)
import Json.Decode as Decode
import Http
import Task exposing (Task)


delayTime : Time
delayTime =
    0.5 * Time.second


getCurrentTime : Task Http.Error String
getCurrentTime =
    Http.get "http://time.jsontest.com/" (Decode.field "time" Decode.string)
        |> Http.toTask


printCurrentTime : Script ()
printCurrentTime =
    Script.attempt getCurrentTime
        |> Script.andThen
            (\result ->
                case result of
                    Ok timeString ->
                        Script.print timeString

                    Err _ ->
                        Script.print "HTTP request failed"
            )


script : Script String
script =
    Script.init { text = "A", number = 2 }
        |> Script.with
            (\model ->
                Script.do
                    [ Script.print model.text
                    , printCurrentTime
                    , Script.sleep delayTime
                    ]
            )
        |> Script.map .number
        |> Script.with (\number -> Script.print number)
        |> Script.aside
            (Script.do
                [ printCurrentTime
                , Script.sleep delayTime
                , Script.attempt getCurrentTime |> Script.ignore
                ]
            )
        |> Script.andThen
            (\number ->
                if number > 2 then
                    Script.succeed "Hooray"
                else
                    Script.fail "Ugh"
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
