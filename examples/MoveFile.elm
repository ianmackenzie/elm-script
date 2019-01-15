module Main exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Context -> Script Int ()
script { workingDirectory } =
    let
        sourceFile =
            workingDirectory |> Directory.file "reversed.txt"

        destinationFile =
            workingDirectory |> Directory.file "reversed-moved.txt"
    in
    File.move sourceFile destinationFile
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
