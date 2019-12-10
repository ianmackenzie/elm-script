module Script.Directory exposing
    ( Directory
    , Error
    , Existence(..)
    , asReadOnly
    , asWriteOnly
    , checkExistence
    , create
    , createTemporary
    , file
    , listFiles
    , listSubdirectories
    , name
    , obliterate
    , remove
    , subdirectory
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Script exposing (Script)
import Script.Internal as Internal
import Script.Path as Path
import Script.Permissions exposing (Read, ReadOnly, ReadWrite, Write, WriteOnly)


type alias Directory p =
    Internal.Directory p


type alias Error =
    { message : String
    }


type Existence
    = Exists
    | DoesNotExist
    | IsNotADirectory


errorDecoder : Decoder Error
errorDecoder =
    Decode.map Error (Decode.field "message" Decode.string)


name : Directory p -> String
name (Internal.Directory path) =
    Path.name path


asReadOnly : Directory (Read p) -> Directory ReadOnly
asReadOnly (Internal.Directory path) =
    Internal.Directory path


asWriteOnly : Directory (Write p) -> Directory WriteOnly
asWriteOnly (Internal.Directory path) =
    Internal.Directory path


subdirectory : String -> Directory p -> Directory p
subdirectory relativePath (Internal.Directory path) =
    Internal.Directory (path ++ [ relativePath ])


file : String -> Directory p -> Internal.File p
file relativePath (Internal.Directory path) =
    Internal.File (path ++ [ relativePath ])


listFiles : Directory (Read p) -> Internal.Script Error (List (Internal.File (Read p)))
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


decodeNullResult : Decoder (Internal.Script Error ())
decodeNullResult =
    Decode.oneOf
        [ Decode.null (Internal.Succeed ())
        , errorDecoder |> Decode.map Internal.Fail
        ]


create : Directory (Write p) -> Internal.Script Error ()
create (Internal.Directory path) =
    Internal.Invoke "createDirectory"
        (Path.encode path)
        decodeNullResult


createTemporary : Internal.Script Error (Directory ReadWrite)
createTemporary =
    Internal.Invoke "createTemporaryDirectory"
        Encode.null
        (Decode.oneOf
            [ Decode.string
                |> Decode.map
                    (List.singleton >> Internal.Directory >> Internal.Succeed)
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )


checkExistence : Directory (Read p) -> Script Error Existence
checkExistence (Internal.Directory path) =
    Path.stat path
        |> Script.map
            (\stat ->
                case stat of
                    Path.Directory ->
                        Exists

                    Path.Nonexistent ->
                        DoesNotExist

                    Path.File ->
                        IsNotADirectory

                    Path.Other ->
                        IsNotADirectory
            )


remove : Directory (Write p) -> Script Error ()
remove (Internal.Directory path) =
    Internal.Invoke "removeDirectory"
        (Path.encode path)
        decodeNullResult


obliterate : Directory (Write p) -> Script Error ()
obliterate (Internal.Directory path) =
    Internal.Invoke "obliterateDirectory"
        (Path.encode path)
        decodeNullResult
