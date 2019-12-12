module ForEach exposing (main)

import Example
import Script exposing (Script)


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
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
