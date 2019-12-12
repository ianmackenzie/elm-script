module ListRecursive exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)


listRecursively : Int -> Directory permissions -> Script Directory.Error ()
listRecursively level directory =
    let
        indentation =
            String.repeat level "    "
    in
    Script.do
        [ Directory.listSubdirectories directory
            |> Script.andThen
                (Script.forEach
                    (\subdirectory ->
                        Script.do
                            [ Script.printLine
                                (indentation ++ Directory.name subdirectory ++ "/")
                            , listRecursively (level + 1) subdirectory
                            ]
                    )
                )
        , Directory.listFiles directory
            |> Script.andThen
                (Script.forEach
                    (\file -> Script.printLine (indentation ++ File.name file))
                )
        ]


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            listRecursively 0 (fileSystem.readOnlyDirectory path)
                |> Script.onError (Example.handleError .message)

        _ ->
            Script.printLine "Please supply one directory name"


main : Script.Program
main =
    Example.program script
