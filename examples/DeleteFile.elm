module DeleteFile exposing (main)

import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { workingDirectory } =
    let
        file =
            File.in_ workingDirectory "reversed.txt"
    in
    File.delete file


main : Script.Program
main =
    Script.program script
