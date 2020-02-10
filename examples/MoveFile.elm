module MoveFile exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        sourceFile =
            File.in_ workingDirectory "reversed.txt"

        destinationFile =
            File.in_ workingDirectory "reversed-moved.txt"
    in
    File.move sourceFile destinationFile


main : Script.Program
main =
    Example.program script
