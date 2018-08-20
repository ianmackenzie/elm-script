module Main exposing (..)

import Common exposing (handleError, requestPort, responsePort)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    Directory.createTemporary
        |> Script.andThen
            (\tempDirectory ->
                let
                    tempFile =
                        tempDirectory |> Directory.file "temp.txt"
                in
                File.writeTo tempFile "dummy contents"
            )
        |> Script.onError (handleError .message)
        |> Script.andThen (\() -> Script.sleep 10000)


main : Script.Program
main =
    Script.program script requestPort responsePort
