module Main exposing (..)

import Example
import Script exposing (Script)
import Script.Directory as Directory


script : Script.Context -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            workingDirectory |> Directory.subdirectory "subdirectory"
    in
    Directory.remove subdirectory
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
