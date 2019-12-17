module CreateFileInSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.subdir workingDirectory "subdirectory"

        file =
            File.in_ subdirectory "child.txt"
    in
    Directory.ensureExists subdirectory
        |> Script.andThen (File.writeTo file "dummy contents")
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
