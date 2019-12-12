module CreateSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    let
        subdirectory =
            workingDirectory |> Directory.subdirectory "subdirectory"
    in
    Directory.create subdirectory
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
