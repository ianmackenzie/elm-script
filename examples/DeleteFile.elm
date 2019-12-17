module DeleteFile exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script Int ()
script { workingDirectory } =
    let
        file =
            File.in_ workingDirectory "reversed.txt"
    in
    File.delete file |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
