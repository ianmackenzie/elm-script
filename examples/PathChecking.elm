port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Context, Script)
import Script.Directory as Directory exposing (Directory)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read)


niceScript : Directory (Read p) -> Script Int ()
niceScript directory =
    Script.readFile (directory |> Directory.file "test.txt")
        |> Script.andThen
            (\contents ->
                Script.print <|
                    toString (String.length contents)
                        ++ " characters in test.txt"
            )
        |> Script.onError (.message >> handleError)


evilScript : Directory (Read p) -> Script Int ()
evilScript directory =
    Script.readFile (directory |> Directory.file "C:/passwords.txt")
        |> Script.ignore
        |> Script.onError (.message >> handleError)


script : Context -> Script Int ()
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
            Script.print "Please supply the path of one directory to read"
                |> Script.andThen (\() -> Script.fail 1)


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
