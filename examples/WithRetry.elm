module Main exposing (main)

import Example
import Script exposing (Script)
import Script.Shell as Shell exposing (Shell)


abort : String -> Script Int a
abort message =
    Script.printLine message
        |> Script.andThen (\() -> Script.fail 1)


retry : Shell -> String -> Int -> Script Int ()
retry shell command count =
    Shell.execute command shell
        |> Script.andThen Script.printLine
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Shell.ProcessExitedWithError _ ->
                            Script.printLine "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() -> retry shell command (count - 1))

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

        _ ->
            let
                command =
                    String.join " " arguments
            in
            retry shell command 5


main : Script.Program
main =
    Example.program script
