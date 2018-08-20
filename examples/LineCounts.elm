module Main exposing (..)

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
            fileSystem |> FileSystem.file path
    in
    List.map toFile arguments
        |> Script.collect getLineCount
        |> Script.onError (Example.handleError .message)
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


main : Script.Program
main =
    Example.program script
