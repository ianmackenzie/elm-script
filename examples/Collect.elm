port module Main exposing (..)

import Kintail.Script as Script exposing (Script)
import Json.Encode exposing (Value)
import Time


script : Script ()
script =
    Script.collect (Script.succeed 3)
        |> Script.andCollect (Script.succeed "four")
        |> Script.andCollect (Script.succeed [ 1, 2, 3, 4, 5 ])
        |> Script.mapCollected
            (\num str lst ->
                num * String.length str * List.length lst
            )
        |> Script.collect
        |> Script.andCollect (Script.perform Time.now)
        |> Script.andThenWithCollected
            (\product time ->
                Script.do
                    [ Script.print ("Product: " ++ toString product)
                    , Script.print
                        ("Current time: " ++ toString (Time.inSeconds time))
                    ]
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
