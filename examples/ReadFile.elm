module ReadFile exposing (main)

import Example
import Script exposing (Script)
import Script.File as File exposing (File)


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    case arguments of
        [ path ] ->
            File.read (host.readOnlyFile path)
                |> Script.map String.lines
                |> Script.map (List.filter (not << String.isEmpty))
                |> Script.andThen
                    (Script.forEach (String.toUpper >> Script.printLine))
                |> Script.onError (Example.handleError .message)

        _ ->
            Script.printLine "Please supply the path of one file to read"
                |> Script.andThen (\() -> Script.fail 1)


main : Script.Program
main =
    Example.program script
