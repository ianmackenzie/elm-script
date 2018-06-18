port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)


script : Script.Context -> Script Int ()
script { arguments } =
    arguments
        |> Script.forEach
            (\argument ->
                Script.printLine <|
                    case String.toFloat argument of
                        Just value ->
                            let
                                squared =
                                    value * value
                            in
                            argument
                                ++ " squared is "
                                ++ String.fromFloat squared

                        Nothing ->
                            argument ++ " is not a number!"
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
