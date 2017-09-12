port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


abort : String -> Script Int ()
abort message =
    Script.do [ Script.print message, Script.fail 1 ]


retry : String -> List String -> Int -> Script Int ()
retry executable arguments maxCount =
    Script.execute executable arguments
        |> Script.andThen Script.print
        |> Script.onError
            (\error ->
                if maxCount > 0 then
                    case error of
                        Script.ProcessExitedWithError _ ->
                            Script.print "Process exited with error, retrying..."
                                |> Script.andThen
                                    (\() ->
                                        retry executable
                                            arguments
                                            (maxCount - 1)
                                    )

                        Script.ProcessWasTerminated ->
                            abort "Process was terminated"

                        Script.ProcessFailed message ->
                            abort ("Process could not be run: " ++ message)
                else
                    abort "Process failed too many times"
            )


script : List String -> Script Int ()
script arguments =
    case arguments of
        executable :: rest ->
            retry executable rest 5

        [] ->
            Script.do
                [ Script.print "Please enter an executable to run"
                , Script.fail 1
                ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
