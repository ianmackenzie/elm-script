module WriteFile exposing (main)

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


script : Script.Init -> Script String ()
script { workingDirectory, platform } =
    let
        inputFile =
            File.in_ workingDirectory "test.txt"

        outputFile =
            File.in_ workingDirectory "reversed.txt"

        lineSeparator =
            Platform.lineSeparator platform
    in
    File.read inputFile
        |> Script.map (reverseLines lineSeparator)
        |> Script.thenWith (\reversedInput -> File.writeTo outputFile reversedInput)


main : Script.Program
main =
    Script.program script
