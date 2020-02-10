module Script.File exposing
    ( Existence(..)
    , File
    , ReadOnly
    , Writable
    , asReadOnly
    , checkExistence
    , copy
    , copyInto
    , delete
    , in_
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
import Script.FileInfo as FileInfo
import Script.Internal as Internal exposing (Directory, Flags, Script(..), UserPrivileges(..))
import Script.Path as Path
import Script.Permissions as Permissions


type alias File permissions =
    Internal.File permissions


type alias ReadOnly =
    Permissions.ReadOnly


type alias Writable =
    Permissions.Writable


type Existence
    = Exists
    | DoesNotExist
    | IsNotAFile


in_ : Directory permissions -> String -> File permissions
in_ (Internal.Directory directoryPath) relativePath =
    Internal.File (Path.append relativePath directoryPath)


readOnly : UserPrivileges -> String -> File ReadOnly
readOnly (UserPrivileges workingDirectoryPath) pathString =
    Internal.File (Path.resolve workingDirectoryPath pathString)


writable : UserPrivileges -> String -> File Writable
writable (UserPrivileges workingDirectoryPath) pathString =
    Internal.File (Path.resolve workingDirectoryPath pathString)


errorDecoder : Decoder String
errorDecoder =
    Decode.field "message" Decode.string


name : File permissions -> String
name (Internal.File filePath) =
    Path.name filePath


asReadOnly : File Writable -> File ReadOnly
asReadOnly (Internal.File filePath) =
    Internal.File filePath


read : File permissions -> Script String String
read (Internal.File filePath) =
    Invoke "readFile" (Path.encode filePath) <|
        \flags ->
            Decode.oneOf
                [ Decode.string |> Decode.map Succeed
                , errorDecoder |> Decode.map Fail
                ]


decodeNullResult : Flags -> Decoder (Script String ())
decodeNullResult flags =
    Decode.oneOf
        [ Decode.null (Succeed ())
        , errorDecoder |> Decode.map Fail
        ]


write : String -> File Writable -> Script String ()
write contents (Internal.File filePath) =
    Invoke "writeFile"
        (Encode.object
            [ ( "contents", Encode.string contents )
            , ( "path", Path.encode filePath )
            ]
        )
        decodeNullResult


writeTo : File Writable -> String -> Script String ()
writeTo file contents =
    write contents file


copy : File permissions -> File Writable -> Script String ()
copy (Internal.File sourcePath) (Internal.File destinationPath) =
    Invoke "copyFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


move : File Writable -> File Writable -> Script String ()
move (Internal.File sourcePath) (Internal.File destinationPath) =
    Invoke "moveFile"
        (Encode.object
            [ ( "sourcePath", Path.encode sourcePath )
            , ( "destinationPath", Path.encode destinationPath )
            ]
        )
        decodeNullResult


delete : File Writable -> Script String ()
delete (Internal.File filePath) =
    Invoke "deleteFile" (Path.encode filePath) decodeNullResult


copyInto : Directory Writable -> File permissions -> Script String (File Writable)
copyInto directory file =
    let
        destination =
            in_ directory (name file)
    in
    copy file destination |> Internal.thenWith (\() -> Internal.Succeed destination)


moveInto : Directory Writable -> File Writable -> Script String (File Writable)
moveInto directory file =
    let
        destination =
            in_ directory (name file)
    in
    move file destination |> Internal.thenWith (\() -> Internal.Succeed destination)


checkExistence : File permissions -> Script String Existence
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


path : File permissions -> String
path (Internal.File filePath) =
    Path.toString filePath
