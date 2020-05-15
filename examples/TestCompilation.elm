module TestCompilation exposing (main)

import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { workingDirectory, userPrivileges } =
    Directory.listFiles workingDirectory
        |> Script.thenWith
            (Script.each
                (\file ->
                    if File.name file |> String.endsWith ".elm" then
                        Script.do
                            [ Script.printLine ("Compiling " ++ File.name file)
                            , Script.executeWith userPrivileges
                                { command = "elm"
                                , arguments = [ "make", "--output", "/dev/null", File.path file ]
                                , workingDirectory = workingDirectory
                                }
                                |> Script.ignoreResult
                                |> Script.onError (\_ -> Script.fail "Elm compilation failed")
                            ]

                    else
                        Script.succeed ()
                )
            )


main : Script.Program
main =
    Script.program script
