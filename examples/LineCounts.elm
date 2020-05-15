module LineCounts exposing (main)

import Script exposing (Script)
import Script.File as File exposing (File, ReadOnly)


getLineCount : File ReadOnly -> Script String Int
getLineCount file =
    File.read file
        |> Script.map (String.trimRight >> String.lines >> List.length)


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    List.map (File.readOnly userPrivileges) arguments
        |> Script.collect getLineCount
        |> Script.map (List.map2 Tuple.pair arguments)
        |> Script.thenWith
            (Script.each
                (\( fileName, lineCount ) ->
                    Script.printLine
                        (fileName
                            ++ ": "
                            ++ String.fromInt lineCount
                            ++ " lines"
                        )
                )
            )


main : Script.Program
main =
    Script.program script
