module EnsureSubdirectory exposing (main)

import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.in_ workingDirectory "subdirectory"
    in
    Directory.ensureExists subdirectory


main : Script.Program
main =
    Script.program script
