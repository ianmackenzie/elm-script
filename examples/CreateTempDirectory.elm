module CreateTempDirectory exposing (main)

import Duration
import Example
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script Int ()
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
        |> Script.onError (Example.handleError .message)


main : Script.Program
main =
    Example.program script
