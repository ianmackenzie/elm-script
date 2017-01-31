module Print exposing (..)

import Kintail.Script as Script exposing (Script)
import Process
import Time
import Task


script : Script String
script =
    Script.init { text = "A", number = 4 }
        |> Script.andThen (Script.print .text)
        |> Script.andThen (Script.sleep (3 * Time.second))
        |> Script.map .number
        |> Script.andThen (Script.print identity)
        |> Script.andThen
            (\number ->
                if number > 2 then
                    Script.succeed "Hooray"
                else
                    Script.fail "Ugh"
            )


main =
    Script.run script
