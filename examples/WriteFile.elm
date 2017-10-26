port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


reverseLines : String -> String
reverseLines input =
    input
        -- Remove trailing newline (having one messes up String.lines)
        |> String.trimRight
        -- Split into lines
        |> String.lines
        -- Remove trailing whitespace on each line (nobody wants that)
        |> List.map String.trimRight
        -- Actually reverse lines
        |> List.reverse
        -- Join back into one string
        |> String.join "\n"
        -- Add back trailing newline (every good file should have one)
        |> (\string -> string ++ "\n")


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        inputFile =
            fileSystem
                |> FileSystem.file Permissions.readOnly "test.txt"

        outputFile =
            fileSystem
                |> FileSystem.file Permissions.writeOnly "reversed.txt"
    in
    File.read inputFile
        |> Script.map reverseLines
        |> Script.andThen (File.writeTo outputFile)
        |> Script.onError (handleError .message)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
