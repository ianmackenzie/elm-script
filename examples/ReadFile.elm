port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            fileSystem
                |> FileSystem.file Permissions.readOnly path
                |> File.read
                |> Script.map String.lines
                |> Script.map (List.filter (not << String.isEmpty))
                |> Script.andThen
                    (Script.forEach (String.toUpper >> Script.print))
                |> Script.onError (.message >> handleError)

        _ ->
            Script.print "Please supply the path of one file to read"
                |> Script.andThen (\() -> Script.fail 1)


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
