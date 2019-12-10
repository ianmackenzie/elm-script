module WithRetry exposing (..)

import Example
import Script exposing (Script)
import Script.Shell as Shell exposing (Shell)


abort : String -> Script Int a
abort message =
    Script.printLine message
        |> Script.andThen (\() -> Script.fail 1)


retry : Shell -> String -> List String -> Int -> Script Int ()
retry shell command arguments count =
    Shell.execute command arguments shell
        |> Script.andThen Script.printLine
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Shell.ProcessExitedWithError _ ->
                            Script.printLine "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() -> retry shell command arguments (count - 1))

                        Shell.ProcessWasTerminated ->
                            abort "Process was terminated"

                        Shell.ProcessFailed message ->
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
