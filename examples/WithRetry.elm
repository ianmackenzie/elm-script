port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)
import Kintail.Script.Shell as Shell exposing (Shell)


abort : String -> Script p Int ()
abort message =
    Script.do [ Script.print message, Script.fail 1 ]


retry : Shell -> String -> Int -> Script Int ()
retry shell command count =
    Shell.execute command shell
        |> Script.andThen Script.print
        |> Script.onError
            (\error ->
                if count > 0 then
                    case error of
                        Shell.ProcessExitedWithError _ ->
                            Script.print "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() -> retry shell command (count - 1))

                        Shell.ProcessWasTerminated ->
                            abort "Process was terminated"

                        Shell.ProcessFailed message ->
                            abort ("Process could not be run: " ++ message)
                else
                    abort "Process failed too many times"
            )


script : Context -> Script Int ()
script { arguments, shell } =
    case arguments of
        [] ->
            Script.print "Please enter an executable to run"
                |> Script.andThen (\() -> Script.fail 1)

        _ ->
            let
                command =
                    String.join " " arguments
            in
            retry shell command 5


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
