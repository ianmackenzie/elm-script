module ObliterateSubdirectory exposing (main)

import Script exposing (Script)
import Script.Directory as Directory


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.in_ workingDirectory "subdirectory"
    in
    Directory.obliterate subdirectory


main : Script.Program
main =
    Script.program script
