port module Example exposing (ensureDirectory, handleError, program)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.Permissions as Permissions exposing (Writable)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("- SCRIPT ERROR - " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


ensureDirectory : Directory Writable -> Script Int ()
ensureDirectory directory =
    let
        name =
            Directory.name directory
    in
    Directory.checkExistence directory
        |> Script.onError (handleError .message)
        |> Script.andThen
            (\existence ->
                case existence of
                    Directory.Exists ->
                        Script.printLine ("'" ++ name ++ "' already exists")

                    Directory.DoesNotExist ->
                        Script.do
                            [ Script.printLine ("Creating '" ++ name ++ "'")
                            , Directory.create directory
                                |> Script.onError (handleError .message)
                            ]

                    Directory.IsNotADirectory ->
                        Script.printLine ("- SCRIPT ERROR - '" ++ name ++ "' exists but is not a directory")
                            |> Script.andThen (\() -> Script.fail 1)
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


program : (Script.Context -> Script Int ()) -> Script.Program
program script =
    Script.program script requestPort responsePort
