module PowerShellTest exposing (main)

import Script exposing (Script)


script : Script.Init -> Script String ()
script { workingDirectory, userPrivileges } =
    Script.executeWith userPrivileges
        { command = "PowerShell"
        , arguments = [ "-Command", "Get-ChildItem", "-Name", "-Path", "*.elm" ]
        , workingDirectory = workingDirectory
        }
        |> Script.map String.lines
        |> Script.map (List.map String.trim)
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.thenWith (Script.each (\fileName -> Script.printLine (String.toUpper fileName)))


main : Script.Program
main =
    Script.program script
