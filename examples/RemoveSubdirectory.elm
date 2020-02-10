module RemoveSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.in_ workingDirectory "subdirectory"
    in
    Directory.remove subdirectory


main : Script.Program
main =
    Example.program script
