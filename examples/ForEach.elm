port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script : List String -> Script {} Int ()
script =
    Script.forEach
        (\argument ->
            Script.print <|
                case String.toFloat argument of
                    Ok value ->
                        argument ++ " squared is " ++ toString (value * value)

                    Err _ ->
                        argument ++ " is not a number!"
        )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
