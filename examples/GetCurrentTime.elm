module GetCurrentTime exposing (main)

import Duration
import Script exposing (Script)
import Time


script : Script.Init -> Script String ()
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
    Script.program script
