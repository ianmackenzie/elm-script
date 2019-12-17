module ObliterateSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory


script : Script.Init -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            Directory.subdir workingDirectory "subdirectory"
    in
    Directory.obliterate subdirectory
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
