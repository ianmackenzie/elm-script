module DeleteFile exposing (main)

import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script :
    List String
    -> Script.WorkingDirectory
    -> Script.Host
    -> Script.UserPrivileges
    -> Script Int ()
script arguments workingDirectory host userPrivileges =
    let
        file =
            workingDirectory |> Directory.file "reversed.txt"
    in
    File.delete file |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
