port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Allowed, Script)
import Time


computeProduct : Script p x Int
computeProduct =
    Script.with (Script.succeed 3)
        |> Script.andWith (Script.succeed "four")
        |> Script.andWith (Script.succeed [ 1, 2, 3, 4, 5 ])
        |> Script.return
            (\num str lst ->
                num * String.length str * List.length lst
            )


script : List String -> Script {} Int ()
script arguments =
    Script.with computeProduct
        |> Script.andWith Script.getCurrentTime
        |> Script.yield
            (\product time ->
                Script.do
                    [ Script.print ("Product: " ++ toString product)
                    , Script.print
                        ("Current time: " ++ toString (Time.inSeconds time))
                    ]
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
