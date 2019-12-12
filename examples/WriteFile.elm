module WriteFile exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File
import Script.Platform as Platform


reverseLines : String -> String -> String
reverseLines lineSeparator input =
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
        |> String.join lineSeparator
        -- Add back trailing newline (every good file should have one)
        |> (\string -> string ++ lineSeparator)


script :
    List String
    -> Script.WorkingDirectory
    -> Script.Host
    -> Script.UserPrivileges
    -> Script Int ()
script arguments workingDirectory host userPrivileges =
    let
        inputFile =
            workingDirectory |> Directory.file "test.txt"

        outputFile =
            workingDirectory |> Directory.file "reversed.txt"

        lineSeparator =
            Platform.lineSeparator host.platform
    in
    File.read inputFile
        |> Script.map (reverseLines lineSeparator)
        |> Script.andThen (File.writeTo outputFile)
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
