module Tests exposing (main)

import Example
import Script exposing (Script, Shell)


runTestCases : Shell -> List ( String, List String, String ) -> Script Int ()
runTestCases shell testCases =
    testCases
        |> Script.forEach
            (\( scriptFileName, arguments, expectedOutput ) ->
                shell.execute "elm-run" (scriptFileName :: arguments)
                    |> Script.onError
                        (\processError ->
                            Script.printLine ("Running '" ++ scriptFileName ++ "' failed")
                                |> Script.andThen (\() -> Script.fail 1)
                        )
                    |> Script.andThen
                        (\output ->
                            if String.trim output == expectedOutput then
                                Script.printLine ("PASSED: " ++ scriptFileName)

                            else
                                Script.printLine
                                    ("FAILED: "
                                        ++ scriptFileName
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
                        ++ String.fromInt (List.length testCases)
                        ++ " tests passed"
            )


script : Script.Context -> Script Int ()
script { shell } =
    runTestCases shell
        [ ( "HelloWorld.elm", [], "Hello World!" )
        , ( "GetElmVersion.elm", [], "Current Elm version: 0.19.0" )
        , ( "LineCounts.elm", [ "test.txt" ], "test.txt: 3 lines" )
        , ( "ForEach.elm"
          , [ "1", "2", "undefined", "3.5" ]
          , "1 squared is 1\n2 squared is 4\nundefined is not a number!\n3.5 squared is 12.25"
          )
        ]


main : Script.Program
main =
    Example.program script
