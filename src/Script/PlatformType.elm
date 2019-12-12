module Script.PlatformType exposing (PlatformType(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type PlatformType
    = Posix
    | Windows


decoder : Decoder PlatformType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "posix" ->
                        Decode.succeed Posix

                    "windows" ->
                        Decode.succeed Windows

                    _ ->
                        Decode.fail ("Unrecognized platform '" ++ string ++ "'")
            )
