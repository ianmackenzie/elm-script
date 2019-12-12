module WithRetry exposing (main)

import Example
import Script exposing (Script)


abort : String -> Script Int a
abort message =
    Script.printLine message
        |> Script.andThen (\() -> Script.fail 1)


retry : Script.Host -> String -> List String -> Int -> Script Int ()
retry host command arguments count =
    host.execute command arguments
        |> Script.andThen Script.printLine
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Script.ExecutableNotFound ->
                            abort "Process executable not found"

                        Script.SubprocessExitedWithError _ ->
                            Script.printLine "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() -> retry host command arguments (count - 1))

                        Script.SubprocessWasTerminated ->
                            abort "Process was terminated"

                        Script.SubprocessFailed message ->
                            abort ("Process could not be run: " ++ message)

                else
                    abort "Process failed too many times"
            )


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    case arguments of
        [] ->
            Script.printLine "Please enter an executable to run"
                |> Script.andThen (\() -> Script.fail 1)

        command :: rest ->
            retry host command rest 5


main : Script.Program
main =
    Example.program script
