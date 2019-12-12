module Script.Internal exposing
    ( Directory(..)
    , Environment(..)
    , File(..)
    , FileSystem(..)
    , Flags
    , NetworkConnection(..)
    , Script(..)
    , UserPrivileges(..)
    )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value)
import Platform.Cmd exposing (Cmd)
import Script.Path exposing (Path)
import Script.Platform exposing (Platform)
import Task exposing (Task)


type alias Flags =
    { arguments : List String
    , platform : Platform
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
    = Environment Platform (Dict String String)


type NetworkConnection
    = NetworkConnection


type UserPrivileges
    = UserPrivileges Path
