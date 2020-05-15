module ReadFile exposing (main)

import Script exposing (Script)
import Script.File as File exposing (File)


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    case arguments of
        [ path ] ->
            File.read (File.readOnly userPrivileges path)
                |> Script.map String.lines
                |> Script.map (List.filter (not << String.isEmpty))
                |> Script.thenWith (Script.each (\line -> Script.printLine (String.toUpper line)))

        _ ->
            Script.fail "Please supply the path of one file to read"


main : Script.Program
main =
    Script.program script
