port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (FileError, Script)


listRecursively : Int -> String -> Script FileError ()
listRecursively level directory =
    let
        indentation =
            String.repeat level "    "
    in
    Script.do
        [ Script.listSubdirectories directory
            |> Script.andThen
                (Script.forEach
                    (\subdirectory ->
                        Script.do
                            [ Script.print
                                (indentation ++ subdirectory ++ "/")
                            , listRecursively (level + 1)
                                (directory ++ "/" ++ subdirectory)
                            ]
                    )
                )
        , Script.listFiles directory
            |> Script.andThen
                (Script.forEach
                    (\filename -> Script.print (indentation ++ filename))
                )
        ]


script : List String -> Script Int ()
script arguments =
    case arguments of
        [ directory ] ->
            listRecursively 0 directory
                |> Script.onError (.message >> handleError)

        _ ->
            Script.print "Please supply one directory name"


handleError : String -> Script Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
