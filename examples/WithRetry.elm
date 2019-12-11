module WithRetry exposing (main)

import Example
import Script exposing (Script, Shell)


abort : String -> Script Int a
abort message =
    Script.printLine message
        |> Script.andThen (\() -> Script.fail 1)


retry : Shell -> String -> List String -> Int -> Script Int ()
retry shell command arguments count =
    shell.execute command arguments
        |> Script.andThen Script.printLine
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Script.SubprocessExitedWithError _ ->
                            Script.printLine "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() -> retry shell command arguments (count - 1))

                        Script.SubprocessWasTerminated ->
                            abort "Process was terminated"

                        Script.SubprocessFailed message ->
                            abort ("Process could not be run: " ++ message)

                else
                    abort "Process failed too many times"
            )


script : Script.Context -> Script Int ()
script { arguments, shell } =
    case arguments of
        [] ->
            Script.printLine "Please enter an executable to run"
                |> Script.andThen (\() -> Script.fail 1)

        command :: rest ->
            retry shell command rest 5


main : Script.Program
main =
    Example.program script
