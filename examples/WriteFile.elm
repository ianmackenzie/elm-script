module WriteFile exposing (..)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


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
script { workingDirectory } =
    let
        inputFile =
            workingDirectory |> Directory.file "test.txt"

        outputFile =
            workingDirectory |> Directory.file "reversed.txt"
    in
    File.read inputFile
        |> Script.map reverseLines
        |> Script.andThen (File.writeTo outputFile)
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
