port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Context, FileError, Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read)


listRecursively : Int -> Directory (Read p) -> Script FileError ()
listRecursively level directory =
    let
        indentation =
            String.repeat level "    "
    in
    Script.do
        [ Script.listSubdirectories directory
            |> Script.andThen
                (Script.forEach
                    (\subdirectory ->
                        Script.do
                            [ Script.print
                                (indentation ++ Directory.name subdirectory ++ "/")
                            , listRecursively (level + 1) subdirectory
                            ]
                    )
                )
        , Script.listFiles directory
            |> Script.andThen
                (Script.forEach
                    (\file -> Script.print (indentation ++ File.name file))
                )
        ]


script : Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            fileSystem
                |> FileSystem.directory Permissions.readOnly path
                |> listRecursively 0
                |> Script.onError (.message >> handleError)

        _ ->
            Script.print "Please supply one directory name"


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
