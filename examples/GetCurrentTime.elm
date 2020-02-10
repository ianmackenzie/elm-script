module GetCurrentTime exposing (main)

import Example
import Script exposing (Script)
import Time
import Duration


script : Script.Init -> Script Int ()
script _ =
    Script.getCurrentTime
        |> Script.thenWith
            (\currentTime ->
                let
                    millisecondsSinceEpoch =
                        toFloat (Time.posixToMillis currentTime)

                    hoursSinceEpoch =
                        Duration.milliseconds millisecondsSinceEpoch
                            |> Duration.inHours
                in
                Script.printLine <|
                    "Number of hours since January 1, 1970: "
                        ++ String.fromFloat hoursSinceEpoch
            )


main : Script.Program
main =
    Example.program script
