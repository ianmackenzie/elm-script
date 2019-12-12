module Script.Internal exposing
    ( Directory(..)
    , Environment(..)
    , File(..)
    , FileSystem(..)
    , Flags
    , NetworkConnection(..)
    , Script(..)
    , Shell(..)
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
    , environment : Environment
    , workingDirectoryPath : Path
    }


type Script x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script x a))
    | Invoke String Value (Flags -> Decoder (Script x a))
    | Do (Cmd (Script x a))


type FileSystem
    = FileSystem


type Directory permissions
    = Directory Path


type File permissions
    = File Path


type Environment
    = Environment PlatformType (Dict String String)


type NetworkConnection
    = NetworkConnection


type Shell
    = Shell
