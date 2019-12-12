module EnsureSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)


script : Script.Context -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            workingDirectory |> Directory.subdirectory "subdirectory"
    in
    Directory.ensureExists subdirectory
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
