module Script.Shell exposing (ProcessError(..), Shell, execute)

import Json.Decode as Decode
import Json.Encode as Encode
import Script.Internal as Internal


type alias Shell =
    Internal.Shell


type ProcessError
    = ProcessFailed String
    | ProcessWasTerminated
    | ProcessExitedWithError Int


execute : String -> Shell -> Internal.Script ProcessError String
execute command shell =
    Internal.Invoke "execute"
        (Encode.string command)
        (Decode.oneOf
            [ Decode.string |> Decode.map Internal.Succeed
            , Decode.field "error" Decode.string
                |> Decode.andThen
                    (\error ->
                        case error of
                            "failed" ->
                                Decode.field "message" Decode.string
                                    |> Decode.map ProcessFailed

                            "terminated" ->
                                Decode.succeed ProcessWasTerminated

                            "exited" ->
                                Decode.field "code" Decode.int
                                    |> Decode.map ProcessExitedWithError

                            _ ->
                                Decode.fail "Unexpected execution error type"
                    )
                |> Decode.map Internal.Fail
            ]
        )
