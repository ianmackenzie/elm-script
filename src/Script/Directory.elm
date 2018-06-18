module Script.Directory
    exposing
        ( Directory
        , Error
        , file
        , listFiles
        , listSubdirectories
        , name
        , subdirectory
        , toReadOnly
        , toWriteOnly
        )

import Json.Decode as Decode exposing (Decoder)
import Script.File as File exposing (File)
import Script.Internal as Internal
import Script.Path as Path
import Script.Permissions exposing (Read, ReadOnly, Write, WriteOnly)


type alias Directory p =
    Internal.Directory p


type alias Error =
    { code : String
    , message : String
    }


errorDecoder : Decoder Error
errorDecoder =
    Decode.map2 Error
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


name : Directory p -> String
name (Internal.Directory path) =
    Path.name path


toReadOnly : Directory (Read p) -> Directory ReadOnly
toReadOnly (Internal.Directory path) =
    Internal.Directory path


toWriteOnly : Directory (Write p) -> Directory WriteOnly
toWriteOnly (Internal.Directory path) =
    Internal.Directory path


subdirectory : String -> Directory p -> Directory p
subdirectory relativePath (Internal.Directory path) =
    Internal.Directory (path ++ [ relativePath ])


file : String -> Directory p -> File p
file relativePath (Internal.Directory path) =
    Internal.File (path ++ [ relativePath ])


listFiles : Directory (Read p) -> Internal.Script Error (List (File (Read p)))
listFiles ((Internal.Directory path) as directory) =
    Internal.Invoke "listFiles"
        (Path.encode path)
        (Decode.oneOf
            [ Decode.list Decode.string
                |> Decode.map (List.map (\fileName -> file fileName directory))
                |> Decode.map Internal.Succeed
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )


listSubdirectories : Directory (Read p) -> Internal.Script Error (List (Directory (Read p)))
listSubdirectories ((Internal.Directory path) as directory) =
    Internal.Invoke "listSubdirectories"
        (Path.encode path)
        (Decode.oneOf
            [ Decode.list Decode.string
                |> Decode.map
                    (List.map
                        (\directoryName -> subdirectory directoryName directory)
                    )
                |> Decode.map Internal.Succeed
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )
