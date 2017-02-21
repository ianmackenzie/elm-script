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


getCurrentTime : Task String String
getCurrentTime =
    Http.get "http://time.jsontest.com/" (Decode.field "time" Decode.string)
        |> Http.toTask
        |> Task.mapError (always "HTTP request failed")


printCurrentTime : Script String ()
printCurrentTime =
    Script.perform getCurrentTime
        |> Script.andThen (\result -> Script.print result)


script : Script String String
script =
    Script.init { text = "A", number = 2 }
        |> Script.aside
            (\model ->
                Script.do
                    [ Script.print model.text
                    , printCurrentTime
                    , Script.sleep delayTime
                    ]
            )
        |> Script.map .number
        |> Script.aside
            (\number ->
                Script.do
                    [ Script.print number
                    , printCurrentTime
                    , Script.sleep delayTime
                    , Script.perform getCurrentTime |> Script.ignore
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
