module Main exposing (..)

import Example
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


main : Script.Program
main =
    Example.program script
