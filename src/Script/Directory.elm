module Script.Directory exposing
    ( Directory
    , Error
    , Existence(..)
    , ReadOnly
    , Writable
    , asReadOnly
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
import Script.Internal as Internal exposing (File(..), Flags, Script(..))
import Script.Path as Path exposing (Path)
import Script.Permissions as Permissions
import Script.PlatformType as PlatformType


type alias Directory permissions =
    Internal.Directory permissions


type alias ReadOnly =
    Permissions.ReadOnly


type alias Writable =
    Permissions.Writable


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


name : Directory permissions -> String
name (Internal.Directory directoryPath) =
    Path.name directoryPath


asReadOnly : Directory Writable -> Directory ReadOnly
asReadOnly (Internal.Directory directoryPath) =
    Internal.Directory directoryPath


subdirectory : String -> Directory permissions -> Directory permissions
subdirectory relativePath (Internal.Directory directoryPath) =
    Internal.Directory (Path.append relativePath directoryPath)


file : String -> Directory permissions -> File permissions
file relativePath (Internal.Directory directoryPath) =
    File (Path.append relativePath directoryPath)


listFiles : Directory permissions -> Script Error (List (File permissions))
listFiles ((Internal.Directory directoryPath) as directory) =
    Invoke "listFiles" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map (List.map (\fileName -> file fileName directory))
                    |> Decode.map Succeed
                , errorDecoder |> Decode.map Fail
                ]


listSubdirectories : Directory permissions -> Script Error (List (Directory permissions))
listSubdirectories ((Internal.Directory directoryPath) as directory) =
    Invoke "listSubdirectories" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map
                        (List.map
                            (\directoryName -> subdirectory directoryName directory)
                        )
                    |> Decode.map Succeed
                , errorDecoder |> Decode.map Fail
                ]


decodeNullResult : Flags -> Decoder (Script Error ())
decodeNullResult flags =
    Decode.oneOf
        [ Decode.null (Succeed ())
        , errorDecoder |> Decode.map Fail
        ]


create : Directory Writable -> Script Error ()
create (Internal.Directory directoryPath) =
    Invoke "createDirectory" (Path.encode directoryPath) decodeNullResult


createTemporary : Script Error (Directory Writable)
createTemporary =
    Invoke "createTemporaryDirectory" Encode.null <|
        \flags ->
            Decode.oneOf
                [ Decode.string
                    |> Decode.map
                        (\pathString ->
                            Succeed <|
                                Internal.Directory (Path.absolute flags.platformType pathString)
                        )
                , errorDecoder |> Decode.map Fail
                ]


checkExistence : Directory permissions -> Script Error Existence
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


remove : Directory Writable -> Script Error ()
remove (Internal.Directory directoryPath) =
    Invoke "removeDirectory" (Path.encode directoryPath) decodeNullResult


obliterate : Directory Writable -> Script Error ()
obliterate (Internal.Directory directoryPath) =
    Invoke "obliterateDirectory" (Path.encode directoryPath) decodeNullResult


path : Directory permissions -> String
path (Internal.Directory directoryPath) =
    Path.toString directoryPath
