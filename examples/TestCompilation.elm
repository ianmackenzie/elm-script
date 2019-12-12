module TestCompilation exposing (main)

import Example exposing (handleError)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Context -> Script Int ()
script { shell, workingDirectory } =
    Directory.listFiles workingDirectory
        |> Script.onError (handleError .message)
        |> Script.andThen
            (Script.forEach
                (\file ->
                    if File.name file |> String.endsWith ".elm" then
                        Script.do
                            [ Script.printLine ("Compiling " ++ File.name file)
                            , shell.execute "elm"
                                [ "make", "--output", "/dev/null", File.path file ]
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
