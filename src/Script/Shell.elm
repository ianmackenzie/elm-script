module Script.Shell exposing
    ( Option
    , ProcessError(..)
    , Shell
    , execute
    , executeWith
    , workingDirectory
    )

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Script.Internal as Internal exposing (Directory)
import Script.Path as Path
import Script.Permissions as Permissions exposing (ReadWrite)


type alias Shell =
    Internal.Shell


type ProcessError
    = ProcessFailed String
    | ProcessWasTerminated
    | ProcessExitedWithError Int


type Option
    = WorkingDirectory (Directory ReadWrite)


executeWith : List Option -> String -> Shell -> Internal.Script ProcessError String
executeWith options command shell =
    Internal.Invoke "execute"
        (Encode.object
            [ ( "command", Encode.string command )
            , ( "options", Encode.object (List.map toField options) )
            ]
        )
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


execute : String -> Shell -> Internal.Script ProcessError String
execute =
    executeWith []


workingDirectory : Directory ReadWrite -> Option
workingDirectory directory =
    WorkingDirectory directory


toField : Option -> ( String, Value )
toField option =
    case option of
        WorkingDirectory (Internal.Directory path) ->
            ( "workingDirectory", Path.encode path )
