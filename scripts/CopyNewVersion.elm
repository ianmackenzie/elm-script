module CopyNewVersion exposing (main)

import Common exposing (handleError)
import Parser exposing ((|.), (|=), Parser)
import Script exposing (Script, UserPrivileges)
import Script.Directory as Directory
import Script.File as File exposing (File, ReadOnly, Writable)


parseNewLine : Parser ()
parseNewLine =
    Parser.oneOf [ Parser.symbol "\n", Parser.symbol "\u{000D}\n" ]


parseVersion : Parser ( Int, Int )
parseVersion =
    Parser.succeed Tuple.pair
        |. Parser.symbol "\"use strict\";"
        |. parseNewLine
        |. parseNewLine
        |. Parser.symbol "const majorVersion = "
        |= Parser.int
        |. Parser.symbol ";"
        |. parseNewLine
        |. Parser.symbol "const minorVersion = "
        |= Parser.int
        |. Parser.symbol ";"
        |. parseNewLine


script : Script.Init -> Script Int ()
script { userPrivileges } =
    let
        runnerFile =
            File.readOnly userPrivileges "C:/Git/ianmackenzie/elm-script/runner/main.js"
    in
    File.read runnerFile
        |> Script.onError (handleError .message)
        |> Script.thenWith
            (\contents ->
                case Parser.run parseVersion contents of
                    Ok version ->
                        copyVersion userPrivileges version runnerFile

                    Err _ ->
                        Script.printLine "Failed to parse main.js"
                            |> Script.andThen (Script.fail 1)
            )


copyVersion : UserPrivileges -> ( Int, Int ) -> File ReadOnly -> Script Int ()
copyVersion userPrivileges ( majorVersion, minorVersion ) runnerFile =
    let
        versionString =
            String.fromInt majorVersion ++ "." ++ String.fromInt minorVersion

        targetFile =
            File.writable userPrivileges
                ("C:/Git/elm-script/elm-script.github.io/" ++ versionString)
    in
    File.checkExistence targetFile
        |> Script.onError (handleError .message)
        |> Script.thenWith
            (\existence ->
                case existence of
                    File.DoesNotExist ->
                        let
                            latestFile =
                                File.writable userPrivileges
                                    "C:/Git/elm-script/elm-script.github.io/latest"
                        in
                        Script.do
                            [ Script.printLine ("Copying new version " ++ versionString)
                            , File.copy runnerFile targetFile
                            , File.copy runnerFile latestFile
                            ]
                            |> Script.onError (handleError .message)

                    _ ->
                        Script.printLine ("Version " ++ versionString ++ " already exists!")
                            |> Script.andThen (Script.fail 1)
            )


main : Script.Program
main =
    Common.program script
