module Print exposing (..)

import Kintail.Script as Script exposing (Script)
import Process
import Time exposing (Time)
import Task


delayTime : Time
delayTime =
    Time.second


script : Script String
script =
    Script.init { text = "A", number = 1 }
        |> Script.print .text
        |> Script.sleep (always delayTime)
        |> Script.map .number
        |> Script.print identity
        |> Script.sleep (always delayTime)
        |> Script.andThen
            (\number ->
                if number > 2 then
                    Script.succeed "Hooray"
                else
                    Script.fail "Ugh"
            )


main =
    Script.run script
