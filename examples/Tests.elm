port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Shell as Shell exposing (Shell)


runTestCases : Shell -> List ( String, String ) -> Script Int ()
runTestCases shell testCases =
    testCases
        |> Script.forEach
            (\( command, expectedOutput ) ->
                Shell.execute command shell
                    |> Script.onError
                        (\processError ->
                            Script.printLine ("Running '" ++ command ++ "' failed")
                                |> Script.andThen (\() -> Script.fail 1)
                        )
                    |> Script.andThen
                        (\output ->
                            if String.trim output == expectedOutput then
                                Script.printLine ("PASSED: " ++ command)
                            else
                                Script.printLine
                                    ("FAILED: "
                                        ++ command
                                        ++ "\n\n"
                                        ++ "Expected output:\n\n"
                                        ++ expectedOutput
                                        ++ "\n\nActual output:\n\n"
                                        ++ String.trim output
                                    )
                                    |> Script.andThen (\() -> Script.fail 1)
                        )
            )
        |> Script.andThen
            (\() ->
                Script.printLine <|
                    "Success! "
                        ++ toString (List.length testCases)
                        ++ " tests passed"
            )


script : Script.Context -> Script Int ()
script { shell } =
    runTestCases shell
        [ ( "elm-run HelloWorld.elm"
          , "Hello World!"
          )
        , ( "elm-run GetElmVersion.elm"
          , "Current Elm version: 0.18.0"
          )
        , ( "elm-run LineCounts.elm test.txt"
          , "test.txt: 3 lines"
          )
        , ( "elm-run ForEach.elm 1 2 undefined 3.5"
          , "1 squared is 1\n2 squared is 4\nundefined is not a number!\n3.5 squared is 12.25"
          )
        ]



-- Boilerplate


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
