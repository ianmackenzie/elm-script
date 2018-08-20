module Main exposing (..)

import Example
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read, ReadOnly)


niceScript : Directory (Read p) -> Script Int ()
niceScript directory =
    File.read (directory |> Directory.file "test.txt")
        |> Script.andThen
            (\contents ->
                Script.printLine <|
                    String.fromInt (String.length contents)
                        ++ " characters in test.txt"
            )
        |> Script.onError (Example.handleError .message)


evilScript : Directory (Read p) -> Script Int ()
evilScript directory =
    File.read (directory |> Directory.file "C:/passwords.txt")
        |> Script.ignore
        |> Script.onError (Example.handleError .message)


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            let
                directory : Directory ReadOnly
                directory =
                    fileSystem |> FileSystem.directory path
            in
            Script.do
                [ niceScript directory
                , evilScript directory
                ]

        _ ->
            Script.printLine "Please supply the path of one directory to read"
                |> Script.andThen (\() -> Script.fail 1)


main : Script.Program
main =
    Example.program script
