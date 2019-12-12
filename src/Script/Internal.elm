module Script.Internal exposing
    ( Directory(..)
    , EnvironmentVariables(..)
    , File(..)
    , FileSystem(..)
    , Flags
    , NetworkConnection(..)
    , Script(..)
    , Shell(..)
    , perform
    )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value)
import Platform.Cmd exposing (Cmd)
import Script.Path exposing (Path)
import Script.PlatformType exposing (PlatformType)
import Task exposing (Task)


type alias Flags =
    { arguments : List String
    , platformType : PlatformType
    , environmentVariables : EnvironmentVariables
    , workingDirectoryPath : Path
    }


type Script x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script x a))
    | Invoke String Value (Flags -> Decoder (Script x a))
    | Do (Cmd (Script x a))


perform : Task x a -> Script x a
perform =
    Task.map Succeed >> Task.onError (Fail >> Task.succeed) >> Perform


type FileSystem
    = FileSystem


type Directory p
    = Directory Path


type File p
    = File Path


type EnvironmentVariables
    = EnvironmentVariables PlatformType (Dict String String)


type NetworkConnection
    = NetworkConnection


type Shell
    = Shell
