module CheckGitStatus exposing (main)

import Example
import Script exposing (Script, UserPrivileges)
import Script.Directory as Directory exposing (Directory, Writable)


checkForUnpushedChanges : Directory Writable -> UserPrivileges -> Script Script.SubprocessError ()
checkForUnpushedChanges directory userPrivileges =
    Script.executeWith userPrivileges
        { command = "git"
        , arguments = [ "log", "@{push}.." ]
        , workingDirectory = directory
        }
        |> Script.andThen
            (\output ->
                if String.isEmpty (String.trim output) then
                    Script.succeed ()

                else
                    Script.printLine output
            )


checkForUncommittedChanges : Directory Writable -> UserPrivileges -> Script Script.SubprocessError ()
checkForUncommittedChanges directory userPrivileges =
    Script.executeWith userPrivileges
        { command = "git"
        , arguments = [ "status" ]
        , workingDirectory = directory
        }
        |> Script.andThen
            (\output ->
                if String.contains "nothing to commit, working tree clean" output then
                    Script.succeed ()

                else
                    Script.printLine output
            )


checkDirectory : Directory Writable -> UserPrivileges -> Script Int ()
checkDirectory directory userPrivileges =
    Script.do
        [ Script.printLine ("Checking " ++ Directory.name directory)
        , checkForUnpushedChanges directory userPrivileges
        , checkForUncommittedChanges directory userPrivileges
        ]
        |> Script.onError
            (\error ->
                Script.printLine "Running Git failed" |> Script.followedBy (Script.fail 1)
            )


script : Script.Init -> Script Int ()
script { arguments, userPrivileges } =
    case arguments of
        [ parentPath ] ->
            let
                parentDirectory =
                    Directory.writable userPrivileges parentPath
            in
            Directory.listSubdirectories parentDirectory
                |> Script.onError (Example.handleError .message)
                |> Script.andThen
                    (Script.forEach
                        (\directory ->
                            Directory.checkExistence (Directory.subdirectory ".git" directory)
                                |> Script.onError (Example.handleError .message)
                                |> Script.andThen
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
            Script.printLine "Please pass a single parent directory to check within"
                |> Script.followedBy (Script.fail 1)


main : Script.Program
main =
    Example.program script
