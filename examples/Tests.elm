module Tests exposing (main)

import Script exposing (Script)
import Script.Directory exposing (Directory, Writable)


runTestCases : Directory Writable -> Script.UserPrivileges -> List ( String, List String, String ) -> Script String ()
runTestCases workingDirectory userPrivileges testCases =
    testCases
        |> Script.each
            (\( scriptFileName, scriptArguments, expectedOutput ) ->
                Script.executeWith userPrivileges
                    { command = "deno"
                    , arguments =
                        [ "run", "-A", "../runner/main.js", "run", scriptFileName ]
                            ++ scriptArguments
                    , workingDirectory = workingDirectory
                    }
                    |> Script.onError
                        (\processError ->
                            Script.fail ("Running '" ++ scriptFileName ++ "' failed")
                        )
                    |> Script.thenWith
                        (\output ->
                            if String.trim output == expectedOutput then
                                Script.printLine ("PASSED: " ++ scriptFileName)

                            else
                                Script.fail
                                    ("FAILED: "
                                        ++ scriptFileName
                                        ++ "\n\n"
                                        ++ "Expected output:\n\n"
                                        ++ expectedOutput
                                        ++ "\n\nActual output:\n\n"
                                        ++ String.trim output
                                    )
                        )
            )
        |> Script.andThen
            (Script.printLine <|
                "Success! "
                    ++ String.fromInt (List.length testCases)
                    ++ " tests passed"
            )


script : Script.Init -> Script String ()
script { workingDirectory, userPrivileges } =
    runTestCases workingDirectory userPrivileges <|
        [ ( "HelloWorld.elm", [], "Hello World!" )
        , ( "GetElmVersion.elm", [], "Current Elm version: 0.19.1" )
        , ( "LineCounts.elm", [ "test.txt" ], "test.txt: 3 lines" )
        , ( "ForEach.elm"
          , [ "1", "2", "undefined", "3.5" ]
          , "1 squared is 1\n2 squared is 4\nundefined is not a number!\n3.5 squared is 12.25"
          )
        ]


main : Script.Program
main =
    Script.program script
