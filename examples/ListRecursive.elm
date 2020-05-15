module ListRecursive exposing (main)

import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)


listRecursively : Int -> Directory permissions -> Script String ()
listRecursively level directory =
    let
        indentation =
            String.repeat level "    "
    in
    Script.do
        [ Directory.listSubdirs directory
            |> Script.thenWith
                (Script.each
                    (\subdirectory ->
                        Script.printLine (indentation ++ Directory.name subdirectory ++ "/")
                            |> Script.andThen (listRecursively (level + 1) subdirectory)
                    )
                )
        , Directory.listFiles directory
            |> Script.thenWith
                (Script.each (\file -> Script.printLine (indentation ++ File.name file)))
        ]


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    case arguments of
        [ path ] ->
            listRecursively 0 (Directory.readOnly userPrivileges path)

        _ ->
            Script.fail "Please supply one directory name"


main : Script.Program
main =
    Script.program script
