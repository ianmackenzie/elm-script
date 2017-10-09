port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)
import Kintail.Script.Process as Process exposing (Process)


script : Process -> Script Int ()
script process =
    Process.arguments process
        |> Script.forEach
            (\argument ->
                Script.print <|
                    case String.toFloat argument of
                        Ok value ->
                            let
                                squared =
                                    value * value
                            in
                            argument ++ " squared is " ++ toString squared

                        Err _ ->
                            argument ++ " is not a number!"
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
