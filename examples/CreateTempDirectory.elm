module CreateTempDirectory exposing (main)

import Duration
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script _ =
    Directory.createTemporary
        |> Script.thenWith
            (\tempDirectory ->
                let
                    tempFile =
                        File.in_ tempDirectory "temp.txt"
                in
                File.writeTo tempFile "dummy contents"
            )
        |> Script.andThen (Script.sleep (Duration.seconds 10))


main : Script.Program
main =
    Script.program script
