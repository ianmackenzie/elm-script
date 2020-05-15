module UsuallyFails exposing (main)

import Script exposing (Script)
import Time


script : Script.Init -> Script String ()
script _ =
    Script.getCurrentTime
        |> Script.thenWith
            (\time ->
                if (Time.posixToMillis time |> modBy 100) > 87 then
                    Script.printLine "Succeeded"

                else
                    Script.fail "Failed"
            )


main : Script.Program
main =
    Script.program script
