module ReadFile exposing (main)

import Example
import Script exposing (Script)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (ReadOnly)


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            let
                inputFile : File ReadOnly
                inputFile =
                    fileSystem |> FileSystem.file path
            in
            File.read inputFile
                |> Script.map String.lines
                |> Script.map (List.filter (not << String.isEmpty))
                |> Script.andThen
                    (Script.forEach (String.toUpper >> Script.printLine))
                |> Script.onError (Example.handleError .message)

        _ ->
            Script.printLine "Please supply the path of one file to read"
                |> Script.andThen (\() -> Script.fail 1)


main : Script.Program
main =
    Example.program script
