module CreateFileInSubdirectory exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Context -> Script Int ()
script { workingDirectory } =
    let
        subdirectory =
            workingDirectory |> Directory.subdirectory "subdirectory"
    in
    Example.ensureDirectory subdirectory
        |> Script.andThen
            (\() ->
                let
                    file =
                        subdirectory |> Directory.file "child.txt"
                in
                File.writeTo file "dummy contents"
                    |> Script.onError (Example.handleError .message)
            )


main : Script.Program
main =
    Example.program script
