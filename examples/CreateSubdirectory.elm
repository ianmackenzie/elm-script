module CreateSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.subdir workingDirectory "subdirectory"
    in
    Directory.create subdirectory


main : Script.Program
main =
    Example.program script
