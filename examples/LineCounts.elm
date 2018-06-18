port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read, ReadOnly)


getLineCount : File (Read p) -> Script File.Error Int
getLineCount file =
    File.read file
        |> Script.map (String.trimRight >> String.lines >> List.length)


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    let
        toFile : String -> File ReadOnly
        toFile path =
            FileSystem.file Permissions.readOnly path fileSystem
    in
    List.map toFile arguments
        |> Script.collect getLineCount
        |> Script.onError (handleError .message)
        |> Script.map (List.map2 Tuple.pair arguments)
        |> Script.andThen
            (Script.forEach
                (\( filename, lineCount ) ->
                    Script.printLine
                        (filename
                            ++ ": "
                            ++ String.fromInt lineCount
                            ++ " lines"
                        )
                )
            )


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
