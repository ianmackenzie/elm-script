module WithRetry exposing (main)

import Script exposing (Script)
import Script.Directory exposing (Directory, Writable)


retry : Directory Writable -> Script.UserPrivileges -> String -> List String -> Int -> Script String ()
retry workingDirectory userPrivileges command arguments count =
    Script.tryToExecuteWith userPrivileges
        { command = command
        , arguments = arguments
        , workingDirectory = workingDirectory
        }
        |> Script.thenWith (\output -> Script.printLine output)
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Script.ExecutableNotFound ->
                            Script.fail "Process executable not found"

                        Script.SubprocessExitedWithError _ ->
                            Script.printLine "Process exited with error, retrying..."
                                |> Script.andThen
                                    (retry
                                        workingDirectory
                                        userPrivileges
                                        command
                                        arguments
                                        (count - 1)
                                    )

                        Script.SubprocessWasTerminated ->
                            Script.fail "Process was terminated"

                        Script.SubprocessFailed message ->
                            Script.fail ("Process could not be run: " ++ message)

                else
                    Script.fail "Process failed too many times"
            )


script : Script.Init -> Script String ()
script { arguments, workingDirectory, userPrivileges } =
    case arguments of
        [] ->
            Script.fail "Please enter an executable to run"

        command :: rest ->
            retry workingDirectory userPrivileges command rest 5


main : Script.Program
main =
    Script.program script
