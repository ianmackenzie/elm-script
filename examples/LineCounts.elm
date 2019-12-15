module LineCounts exposing (main)

import Example
import Script exposing (Script)
import Script.File as File exposing (File)


getLineCount : File permissions -> Script File.Error Int
getLineCount file =
    File.read file
        |> Script.map (String.trimRight >> String.lines >> List.length)


script : Script.Init -> Script Int ()
script { arguments, userPrivileges } =
    List.map (File.readOnly userPrivileges) arguments
        |> Script.collect getLineCount
        |> Script.onError (Example.handleError .message)
        |> Script.map (List.map2 Tuple.pair arguments)
        |> Script.thenWith
            (Script.each
                (\( filename, lineCount ) ->
                    Script.printLine
                        (filename
                            ++ ": "
                            ++ String.fromInt lineCount
                            ++ " lines"
                        )
                )
            )


main : Script.Program
main =
    Example.program script
