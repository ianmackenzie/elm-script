module Script.File exposing
    ( Error
    , Existence(..)
    , File
    , asReadOnly
    , asWriteOnly
    , checkExistence
    , copy
    , copyInto
    , delete
    , move
    , moveInto
    , name
    , read
    , write
    , writeTo
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Script
import Script.Directory as Directory exposing (Directory)
import Script.Internal as Internal
import Script.Path as Path
import Script.Permissions exposing (Read, ReadOnly, ReadWrite, Write, WriteOnly)


type alias File p =
    Internal.File p


type alias Error =
    { message : String
    }


type Existence
    = Exists
    | DoesNotExist
    | IsNotAFile


errorDecoder : Decoder Error
errorDecoder =
    Decode.map Error (Decode.field "message" Decode.string)


name : File p -> String
name (Internal.File path) =
    Path.name path


asReadOnly : File (Read p) -> File ReadOnly
asReadOnly (Internal.File path) =
    Internal.File path


asWriteOnly : File (Write p) -> File WriteOnly
asWriteOnly (Internal.File path) =
    Internal.File path


read : File (Read p) -> Internal.Script Error String
read (Internal.File path) =
    Internal.Invoke "readFile"
        (Path.encode path)
        (Decode.oneOf
            [ Decode.string |> Decode.map Internal.Succeed
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )


decodeNullResult : Decoder (Internal.Script Error ())
decodeNullResult =
    Decode.oneOf
        [ Decode.null (Internal.Succeed ())
        , errorDecoder |> Decode.map Internal.Fail
        ]


write : String -> File (Write p) -> Internal.Script Error ()
write contents (Internal.File path) =
    Internal.Invoke "writeFile"
        (Encode.object
            [ ( "contents", Encode.string contents )
            , ( "path", Path.encode path )
            ]
        )
        decodeNullResult


writeTo : File (Write p) -> String -> Internal.Script Error ()
writeTo file contents =
    write contents file


copy : File (Read sourcePermissions) -> File (Write destinationPermissions) -> Internal.Script Error ()
copy (Internal.File sourcePath) (Internal.File destinationPath) =
    Internal.Invoke "copyFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


move : File ReadWrite -> File (Write destinationPermissions) -> Internal.Script Error ()
move (Internal.File sourcePath) (Internal.File destinationPath) =
    Internal.Invoke "moveFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


delete : File (Write p) -> Internal.Script Error ()
delete (Internal.File path) =
    Internal.Invoke "deleteFile" (Path.encode path) decodeNullResult


copyInto : Directory (Write directoryPermissions) -> File (Read filePermissions) -> Internal.Script Error (File (Write directoryPermissions))
copyInto directory file =
    let
        destination =
            Directory.file (name file) directory
    in
    copy file destination |> Script.andThen (\() -> Script.succeed destination)


moveInto : Directory (Write directoryPermissions) -> File ReadWrite -> Internal.Script Error (File (Write directoryPermissions))
moveInto directory file =
    let
        destination =
            Directory.file (name file) directory
    in
    move file destination |> Script.andThen (\() -> Script.succeed destination)


checkExistence : File (Read p) -> Internal.Script Error Existence
checkExistence (Internal.File path) =
    Path.stat path
        |> Script.map
            (\stat ->
                case stat of
                    Path.File ->
                        Exists

                    Path.Nonexistent ->
                        DoesNotExist

                    Path.Directory ->
                        IsNotAFile

                    Path.Other ->
                        IsNotAFile
            )
