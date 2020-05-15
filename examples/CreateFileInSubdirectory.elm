module CreateFileInSubdirectory exposing (main)

import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.in_ workingDirectory "subdirectory"

        file =
            File.in_ subdirectory "child.txt"
    in
    Directory.ensureExists subdirectory
        |> Script.andThen (File.writeTo file "dummy contents")


main : Script.Program
main =
    Script.program script
