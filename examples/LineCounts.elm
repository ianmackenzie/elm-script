module LineCounts exposing (main)

import Example
import Script exposing (Script)
import Script.File as File exposing (File)


getLineCount : File permissions -> Script File.Error Int
getLineCount file =
    File.read file
        |> Script.map (String.trimRight >> String.lines >> List.length)


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    List.map host.readOnlyFile arguments
        |> Script.collect getLineCount
        |> Script.onError (Example.handleError .message)
        |> Script.map (List.map2 Tuple.pair arguments)
        |> Script.andThen
            (Script.forEach
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
