module CopyFile exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script Int ()
script { workingDirectory } =
    let
        sourceFile =
            File.in_ workingDirectory "reversed.txt"

        destinationFile =
            File.in_ workingDirectory "reversed-copied.txt"
    in
    File.copy sourceFile destinationFile
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
