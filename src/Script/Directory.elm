module Script.Directory
    exposing
        ( Directory
        , Error
        , file
        , listFiles
        , listSubdirectories
        , name
        , subdirectory
        )

import Json.Decode as Decode exposing (Decoder)
import Script.File as File exposing (File)
import Script.Internal as Internal
import Script.Path as Path
import Script.Permissions exposing (Read)


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
                |> Decode.map (List.map (\name -> file name directory))
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
                |> Decode.map (List.map (\name -> subdirectory name directory))
                |> Decode.map Internal.Succeed
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )
