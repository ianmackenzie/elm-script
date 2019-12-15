module Script.File exposing
    ( Error
    , Existence(..)
    , File
    , ReadOnly
    , Writable
    , asReadOnly
    , checkExistence
    , copy
    , copyInto
    , delete
    , move
    , moveInto
    , name
    , path
    , read
    , readOnly
    , writable
    , write
    , writeTo
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Script.Directory as Directory exposing (Directory)
import Script.FileInfo as FileInfo
import Script.Internal as Internal exposing (Flags, Script(..), UserPrivileges(..))
import Script.Path as Path
import Script.Permissions as Permissions


type alias File permissions =
    Internal.File permissions


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
    | IsNotAFile


readOnly : UserPrivileges -> String -> File ReadOnly
readOnly (UserPrivileges workingDirectoryPath) pathString =
    Internal.File (Path.resolve workingDirectoryPath pathString)


writable : UserPrivileges -> String -> File Writable
writable (UserPrivileges workingDirectoryPath) pathString =
    Internal.File (Path.resolve workingDirectoryPath pathString)


errorDecoder : Decoder Error
errorDecoder =
    Decode.map Error (Decode.field "message" Decode.string)


name : File permissions -> String
name (Internal.File filePath) =
    Path.name filePath


asReadOnly : File Writable -> File ReadOnly
asReadOnly (Internal.File filePath) =
    Internal.File filePath


read : File permissions -> Script Error String
read (Internal.File filePath) =
    Invoke "readFile" (Path.encode filePath) <|
        \flags ->
            Decode.oneOf
                [ Decode.string |> Decode.map Succeed
                , errorDecoder |> Decode.map Fail
                ]


decodeNullResult : Flags -> Decoder (Script Error ())
decodeNullResult flags =
    Decode.oneOf
        [ Decode.null (Succeed ())
        , errorDecoder |> Decode.map Fail
        ]


write : String -> File Writable -> Script Error ()
write contents (Internal.File filePath) =
    Invoke "writeFile"
        (Encode.object
            [ ( "contents", Encode.string contents )
            , ( "path", Path.encode filePath )
            ]
        )
        decodeNullResult


writeTo : File Writable -> String -> Script Error ()
writeTo file contents =
    write contents file


copy : File permissions -> File Writable -> Script Error ()
copy (Internal.File sourcePath) (Internal.File destinationPath) =
    Invoke "copyFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


move : File Writable -> File Writable -> Script Error ()
move (Internal.File sourcePath) (Internal.File destinationPath) =
    Invoke "moveFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


delete : File Writable -> Script Error ()
delete (Internal.File filePath) =
    Invoke "deleteFile" (Path.encode filePath) decodeNullResult


copyInto : Directory Writable -> File permissions -> Script Error (File Writable)
copyInto directory file =
    let
        destination =
            Directory.file (name file) directory
    in
    copy file destination |> Internal.thenWith (\() -> Internal.Succeed destination)


moveInto : Directory Writable -> File Writable -> Script Error (File Writable)
moveInto directory file =
    let
        destination =
            Directory.file (name file) directory
    in
    move file destination |> Internal.thenWith (\() -> Internal.Succeed destination)


checkExistence : File permissions -> Script Error Existence
checkExistence (Internal.File filePath) =
    FileInfo.get filePath
        |> Internal.map
            (\fileInfo ->
                case fileInfo of
                    FileInfo.File ->
                        Exists

                    FileInfo.Nonexistent ->
                        DoesNotExist

                    FileInfo.Directory ->
                        IsNotAFile

                    FileInfo.Other ->
                        IsNotAFile
            )
        |> Internal.mapError Error


path : File permissions -> String
path (Internal.File filePath) =
    Path.toString filePath
