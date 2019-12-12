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
    , path
    , remove
    , subdirectory
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Script exposing (Script)
import Script.FileInfo as FileInfo
import Script.Internal as Internal exposing (Flags)
import Script.Path as Path exposing (Path)
import Script.Permissions exposing (Read, ReadOnly, Writable, Write, WriteOnly)
import Script.PlatformType as PlatformType


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
name (Internal.Directory directoryPath) =
    Path.name directoryPath


asReadOnly : Directory (Read p) -> Directory ReadOnly
asReadOnly (Internal.Directory directoryPath) =
    Internal.Directory directoryPath


asWriteOnly : Directory (Write p) -> Directory WriteOnly
asWriteOnly (Internal.Directory directoryPath) =
    Internal.Directory directoryPath


subdirectory : String -> Directory p -> Directory p
subdirectory relativePath (Internal.Directory directoryPath) =
    Internal.Directory (Path.append relativePath directoryPath)


file : String -> Directory p -> Internal.File p
file relativePath (Internal.Directory directoryPath) =
    Internal.File (Path.append relativePath directoryPath)


listFiles : Directory (Read p) -> Internal.Script Error (List (Internal.File (Read p)))
listFiles ((Internal.Directory directoryPath) as directory) =
    Internal.Invoke "listFiles" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map (List.map (\fileName -> file fileName directory))
                    |> Decode.map Internal.Succeed
                , errorDecoder |> Decode.map Internal.Fail
                ]


listSubdirectories : Directory (Read p) -> Internal.Script Error (List (Directory (Read p)))
listSubdirectories ((Internal.Directory directoryPath) as directory) =
    Internal.Invoke "listSubdirectories" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map
                        (List.map
                            (\directoryName -> subdirectory directoryName directory)
                        )
                    |> Decode.map Internal.Succeed
                , errorDecoder |> Decode.map Internal.Fail
                ]


decodeNullResult : Internal.Flags -> Decoder (Internal.Script Error ())
decodeNullResult flags =
    Decode.oneOf
        [ Decode.null (Internal.Succeed ())
        , errorDecoder |> Decode.map Internal.Fail
        ]


create : Directory (Write p) -> Internal.Script Error ()
create (Internal.Directory directoryPath) =
    Internal.Invoke "createDirectory" (Path.encode directoryPath) decodeNullResult


createTemporary : Internal.Script Error (Directory Writable)
createTemporary =
    Internal.Invoke "createTemporaryDirectory" Encode.null <|
        \flags ->
            Decode.oneOf
                [ Decode.string
                    |> Decode.map
                        (\pathString ->
                            Internal.Succeed <|
                                Internal.Directory (Path.absolute flags.platformType pathString)
                        )
                , errorDecoder |> Decode.map Internal.Fail
                ]


checkExistence : Directory (Read p) -> Script Error Existence
checkExistence (Internal.Directory directoryPath) =
    FileInfo.get directoryPath
        |> Script.map
            (\fileInfo ->
                case fileInfo of
                    FileInfo.Directory ->
                        Exists

                    FileInfo.Nonexistent ->
                        DoesNotExist

                    FileInfo.File ->
                        IsNotADirectory

                    FileInfo.Other ->
                        IsNotADirectory
            )
        |> Script.mapError Error


remove : Directory (Write p) -> Script Error ()
remove (Internal.Directory directoryPath) =
    Internal.Invoke "removeDirectory" (Path.encode directoryPath) decodeNullResult


obliterate : Directory (Write p) -> Script Error ()
obliterate (Internal.Directory directoryPath) =
    Internal.Invoke "obliterateDirectory" (Path.encode directoryPath) decodeNullResult


path : Directory p -> String
path (Internal.Directory directoryPath) =
    Path.toString directoryPath
