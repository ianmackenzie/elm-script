module With exposing (main)

import Example
import Script exposing (Script)
import Time


computeProduct : Script x Int
computeProduct =
    Script.with (Script.succeed 3)
        |> Script.andWith (Script.succeed "four")
        |> Script.andWith (Script.succeed [ 1, 2, 3, 4, 5 ])
        |> Script.return
            (\number string list ->
                number * String.length string * List.length list
            )


script : Script.Context -> Script Int ()
script context =
    Script.with computeProduct
        |> Script.andWith Script.getCurrentTime
        |> Script.yield
            (\product time ->
                Script.do
                    [ Script.printLine ("Product: " ++ String.fromInt product)
                    , Script.printLine
                        ("Current time: "
                            ++ String.fromInt (Time.posixToMillis time)
                        )
                    ]
            )


main : Script.Program
main =
    Example.program script
