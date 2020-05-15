module CheckGitStatus exposing (main)

import Script exposing (Script, UserPrivileges)
import Script.Directory as Directory exposing (Directory, Writable)


checkForUnpushedChanges : Directory Writable -> UserPrivileges -> Script String ()
checkForUnpushedChanges directory userPrivileges =
    Script.executeWith userPrivileges
        { command = "git"
        , arguments = [ "log", "@{push}.." ]
        , workingDirectory = directory
        }
        |> Script.thenWith
            (\output ->
                if String.isEmpty (String.trim output) then
                    Script.succeed ()

                else
                    Script.printLine output
            )


checkForUncommittedChanges : Directory Writable -> UserPrivileges -> Script String ()
checkForUncommittedChanges directory userPrivileges =
    Script.executeWith userPrivileges
        { command = "git"
        , arguments = [ "status" ]
        , workingDirectory = directory
        }
        |> Script.thenWith
            (\output ->
                if String.contains "nothing to commit, working tree clean" output then
                    Script.succeed ()

                else
                    Script.printLine output
            )


checkDirectory : Directory Writable -> UserPrivileges -> Script String ()
checkDirectory directory userPrivileges =
    Script.do
        [ Script.printLine ("Checking " ++ Directory.name directory)
        , checkForUnpushedChanges directory userPrivileges
        , checkForUncommittedChanges directory userPrivileges
        ]


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    case arguments of
        [ parentPath ] ->
            let
                parentDirectory =
                    Directory.writable userPrivileges parentPath
            in
            Directory.listSubdirs parentDirectory
                |> Script.thenWith
                    (Script.each
                        (\directory ->
                            Directory.checkExistence (Directory.in_ directory ".git")
                                |> Script.thenWith
                                    (\existence ->
                                        case existence of
                                            Directory.Exists ->
                                                checkDirectory directory userPrivileges

                                            _ ->
                                                Script.succeed ()
                                    )
                        )
                    )

        _ ->
            Script.fail "Please pass a single parent directory to check within"


main : Script.Program
main =
    Script.program script
