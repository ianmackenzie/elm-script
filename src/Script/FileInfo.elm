module Script.FileInfo exposing (FileInfo(..), get)

import Json.Decode as Decode exposing (Decoder)
import Script.Internal as Internal
import Script.Path as Path exposing (Path)


type FileInfo
    = Nonexistent
    | File
    | Directory
    | Other


decodeFileInfo : Decoder FileInfo
decodeFileInfo =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "nonexistent" ->
                        Decode.succeed Nonexistent

                    "file" ->
                        Decode.succeed File

                    "directory" ->
                        Decode.succeed Directory

                    "other" ->
                        Decode.succeed Other

                    _ ->
                        Decode.fail ("Unrecognized stat value '" ++ string ++ "'")
            )


decodeErrorMessage : Decoder String
decodeErrorMessage =
    Decode.field "message" Decode.string


get : Path -> Internal.Script String FileInfo
get path =
    Internal.Invoke "stat"
        (Path.encode path)
        (\flags ->
            Decode.oneOf
                [ decodeFileInfo |> Decode.map Internal.Succeed
                , decodeErrorMessage |> Decode.map Internal.Fail
                ]
        )
