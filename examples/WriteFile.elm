port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)
import Kintail.Script.FileSystem as FileSystem
import Kintail.Script.Permissions as Permissions


script : Context -> Script Int ()
script { fileSystem } =
    let
        inputFile =
            fileSystem
                |> FileSystem.file Permissions.readOnly "test.txt"

        outputFile =
            fileSystem
                |> FileSystem.file Permissions.writeOnly "reversed.txt"
    in
    Script.readFile inputFile
        |> Script.map String.lines
        |> Script.map List.reverse
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.map (String.join "\n")
        |> Script.andThen (Script.writeFile outputFile)
        |> Script.onError (.message >> handleError)


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
