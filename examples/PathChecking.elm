module PathChecking exposing (main)

import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File


niceScript : Directory permissions -> Script String ()
niceScript directory =
    File.read (File.in_ directory "test.txt")
        |> Script.thenWith
            (\contents ->
                Script.printLine <|
                    String.fromInt (String.length contents)
                        ++ " characters in test.txt"
            )


evilScript : Directory permissions -> Script String ()
evilScript directory =
    -- Attempt to sneakily break into a parent directory
    File.read (File.in_ directory "subdirectory/../../test.txt")
        |> Script.ignoreResult


script : Script.Init -> Script String ()
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
            Script.fail "Please supply the path of one directory to read"


main : Script.Program
main =
    Script.program script
