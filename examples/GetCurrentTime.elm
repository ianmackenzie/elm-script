module GetCurrentTime exposing (main)

import Example
import Script exposing (Script)
import Time


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    Script.getCurrentTime
        |> Script.andThen
            (\currentTime ->
                let
                    millisecondsSinceEpoch =
                        toFloat (Time.posixToMillis currentTime)

                    hoursSinceEpoch =
                        millisecondsSinceEpoch / (1000 * 60 * 60)
                in
                Script.printLine <|
                    "Number of hours since January 1, 1970: "
                        ++ String.fromFloat hoursSinceEpoch
            )


main : Script.Program
main =
    Example.program script
