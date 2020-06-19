module CopyNewVersion exposing (main)

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
        |. Parser.symbol "// Used by CopyNewVersion.elm"
        |. parseNewLine
        |. Parser.symbol "const majorVersion = "
        |= Parser.int
        |. Parser.symbol ";"
        |. parseNewLine
        |. Parser.symbol "const minorVersion = "
        |= Parser.int
        |. Parser.symbol ";"
        |. parseNewLine


script : Script.Init -> Script String ()
script { userPrivileges } =
    let
        runnerFile =
            File.readOnly userPrivileges "/home/ian/git/ianmackenzie/elm-script/runner/main.js"
    in
    File.read runnerFile
        |> Script.thenWith
            (\contents ->
                case Parser.run parseVersion contents of
                    Ok version ->
                        copyVersion userPrivileges version runnerFile

                    Err _ ->
                        Script.fail "Failed to parse main.js"
            )


copyVersion : UserPrivileges -> ( Int, Int ) -> File ReadOnly -> Script String ()
copyVersion userPrivileges ( majorVersion, minorVersion ) runnerFile =
    let
        versionString =
            String.fromInt majorVersion ++ "." ++ String.fromInt minorVersion

        targetFile =
            File.writable userPrivileges
                ("/home/ian/git/elm-script/elm-script.github.io/" ++ versionString)
    in
    File.checkExistence targetFile
        |> Script.thenWith
            (\existence ->
                case existence of
                    File.DoesNotExist ->
                        let
                            latestFile =
                                File.writable userPrivileges
                                    "/home/ian/git/elm-script/elm-script.github.io/latest"
                        in
                        Script.do
                            [ Script.printLine ("Copying new version " ++ versionString)
                            , File.copy runnerFile targetFile
                            , File.copy runnerFile latestFile
                            ]

                    _ ->
                        Script.fail ("Version " ++ versionString ++ " already exists!")
            )


main : Script.Program
main =
    Script.program script
