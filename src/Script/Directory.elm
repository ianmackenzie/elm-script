module Script.Directory exposing
    ( Directory
    , Existence(..)
    , ReadOnly
    , Writable
    , asReadOnly
    , checkExistence
    , create
    , createTemporary
    , ensureExists
    , in_
    , listFiles
    , listSubdirs
    , name
    , obliterate
    , path
    , readOnly
    , remove
    , writable
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Script.File as File
import Script.FileInfo as FileInfo
import Script.Internal as Internal exposing (File(..), Flags, Script(..), UserPrivileges(..))
import Script.Path as Path exposing (Path)
import Script.Permissions as Permissions
import Script.Platform as Platform


type alias Directory permissions =
    Internal.Directory permissions


type alias ReadOnly =
    Permissions.ReadOnly


type alias Writable =
    Permissions.Writable


type Existence
    = Exists
    | DoesNotExist
    | IsNotADirectory


readOnly : UserPrivileges -> String -> Directory ReadOnly
readOnly (UserPrivileges workingDirectoryPath) pathString =
    Internal.Directory (Path.resolve workingDirectoryPath pathString)


writable : UserPrivileges -> String -> Directory Writable
writable (UserPrivileges workingDirectoryPath) pathString =
    Internal.Directory (Path.resolve workingDirectoryPath pathString)


errorDecoder : Decoder String
errorDecoder =
    Decode.field "message" Decode.string


name : Directory permissions -> String
name (Internal.Directory directoryPath) =
    Path.name directoryPath


asReadOnly : Directory Writable -> Directory ReadOnly
asReadOnly (Internal.Directory directoryPath) =
    Internal.Directory directoryPath


in_ : Directory permissions -> String -> Directory permissions
in_ (Internal.Directory directoryPath) relativePath =
    Internal.Directory (Path.append relativePath directoryPath)


listFiles : Directory permissions -> Script String (List (File permissions))
listFiles ((Internal.Directory directoryPath) as directory) =
    Invoke "listFiles" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map (List.map (File.in_ directory))
                    |> Decode.map Succeed
                , errorDecoder |> Decode.map Fail
                ]


listSubdirs : Directory permissions -> Script String (List (Directory permissions))
listSubdirs ((Internal.Directory directoryPath) as directory) =
    Invoke "listSubdirectories" (Path.encode directoryPath) <|
        \flags ->
            Decode.oneOf
                [ Decode.list Decode.string
                    |> Decode.map
                        (\names ->
                            Succeed (List.map (in_ directory) names)
                        )
                , errorDecoder |> Decode.map Fail
                ]


decodeNullResult : Flags -> Decoder (Script String ())
decodeNullResult flags =
    Decode.oneOf
        [ Decode.null (Succeed ())
        , errorDecoder |> Decode.map Fail
        ]


create : Directory Writable -> Script String ()
create =
    createDirectory { recursive = False }


ensureExists : Directory Writable -> Script String ()
ensureExists =
    createDirectory { recursive = True }


createDirectory : { recursive : Bool } -> Directory Writable -> Script String ()
createDirectory { recursive } (Internal.Directory directoryPath) =
    Invoke "createDirectory"
        (Encode.object
            [ ( "path", Path.encode directoryPath )
            , ( "recursive", Encode.bool recursive )
            ]
        )
        decodeNullResult


createTemporary : Script String (Directory Writable)
createTemporary =
    Invoke "createTemporaryDirectory" Encode.null <|
        \flags ->
            Decode.oneOf
                [ Decode.string
                    |> Decode.map
                        (\pathString ->
                            Succeed <|
                                Internal.Directory (Path.absolute flags.platform pathString)
                        )
                , errorDecoder |> Decode.map Fail
                ]


checkExistence : Directory permissions -> Script String Existence
checkExistence (Internal.Directory directoryPath) =
    FileInfo.get directoryPath
        |> Internal.map
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


remove : Directory Writable -> Script String ()
remove =
    removeDirectory { recursive = False }


obliterate : Directory Writable -> Script String ()
obliterate =
    removeDirectory { recursive = True }


removeDirectory : { recursive : Bool } -> Directory Writable -> Script String ()
removeDirectory { recursive } (Internal.Directory directoryPath) =
    Invoke "removeDirectory"
        (Encode.object
            [ ( "path", Path.encode directoryPath )
            , ( "recursive", Encode.bool recursive )
            ]
        )
        decodeNullResult


path : Directory permissions -> String
path (Internal.Directory directoryPath) =
    Path.toString directoryPath
