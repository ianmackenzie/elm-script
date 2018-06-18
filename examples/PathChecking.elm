port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read)


niceScript : Directory (Read p) -> Script Int ()
niceScript directory =
    File.read (directory |> Directory.file "test.txt")
        |> Script.andThen
            (\contents ->
                Script.printLine <|
                    String.fromInt (String.length contents)
                        ++ " characters in test.txt"
            )
        |> Script.onError (handleError .message)


evilScript : Directory (Read p) -> Script Int ()
evilScript directory =
    File.read (directory |> Directory.file "C:/passwords.txt")
        |> Script.ignore
        |> Script.onError (handleError .message)


script : Script.Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            let
                directory =
                    fileSystem
                        |> FileSystem.directory Permissions.readOnly path
            in
            Script.do
                [ niceScript directory
                , evilScript directory
                ]

        _ ->
            Script.printLine "Please supply the path of one directory to read"
                |> Script.andThen (\() -> Script.fail 1)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
