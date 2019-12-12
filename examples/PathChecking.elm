module PathChecking exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File


niceScript : Directory permissions -> Script Int ()
niceScript directory =
    File.read (directory |> Directory.file "test.txt")
        |> Script.andThen
            (\contents ->
                Script.printLine <|
                    String.fromInt (String.length contents)
                        ++ " characters in test.txt"
            )
        |> Script.onError (Example.handleError .message)


evilScript : Directory permissions -> Script Int ()
evilScript directory =
    File.read (directory |> Directory.file "C:/passwords.txt")
        |> Script.ignoreResult
        |> Script.onError (Example.handleError .message)


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            let
                directory =
                    fileSystem.readOnlyDirectory path
            in
            Script.do
                [ niceScript directory
                , evilScript directory
                ]

        _ ->
            Script.printLine "Please supply the path of one directory to read"
                |> Script.andThen (\() -> Script.fail 1)


main : Script.Program
main =
    Example.program script
