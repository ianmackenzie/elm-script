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
    -- Attempt to sneakily break into a parent directory
    File.read (directory |> Directory.file "subdirectory/../../test.txt")
        |> Script.ignoreResult
        |> Script.onError (Example.handleError .message)


script : Script.Init -> Script Int ()
script { arguments, userPrivileges } =
    case arguments of
        [ path ] ->
            let
                directory =
                    Directory.readOnly userPrivileges path
            in
            Script.do
                [ niceScript directory
                , evilScript directory
                ]

        _ ->
            Script.printLine "Please supply the path of one directory to read"
                |> Script.followedBy (Script.fail 1)


main : Script.Program
main =
    Example.program script
