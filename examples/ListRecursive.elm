port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (FileError, Script)
import Kintail.Script.Directory as Directory exposing (Directory)
import Kintail.Script.File as File exposing (File)
import Kintail.Script.FileSystem as FileSystem
import Kintail.Script.Permissions exposing (Allowed(Allowed))
import Kintail.Script.Process as Process exposing (Process)


listRecursively : Int -> Directory { p | read : Allowed } -> Script FileError ()
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


script : Process -> Script Int ()
script process =
    case Process.arguments process of
        [ path ] ->
            let
                fileSystem =
                    Process.fileSystem process

                directory =
                    FileSystem.directory { read = Allowed } path (Process.fileSystem process)
            in
            listRecursively 0 directory
                |> Script.onError (.message >> handleError)

        _ ->
            Script.print "Please supply one directory name"


handleError : String -> Script Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
