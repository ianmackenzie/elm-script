module Main exposing (..)

import Example
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)


script : Script.Context -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            workingDirectory |> Directory.subdirectory "subdirectory"
    in
    Example.ensureDirectory subdirectory


main : Script.Program
main =
    Example.program script
