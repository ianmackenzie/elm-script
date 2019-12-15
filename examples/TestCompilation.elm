module TestCompilation exposing (main)

import Example exposing (handleError)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script Int ()
script { workingDirectory, userPrivileges } =
    Directory.listFiles workingDirectory
        |> Script.onError (handleError .message)
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
                                |> Script.onError (\error -> Script.fail 1)
                            ]

                    else
                        Script.do []
                )
            )


main : Script.Program
main =
    Example.program script
