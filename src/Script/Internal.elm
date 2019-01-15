module Script.Internal exposing
    ( Directory(..)
    , EnvironmentVariables(..)
    , File(..)
    , FileSystem(..)
    , NetworkConnection(..)
    , Path
    , Script(..)
    , Shell(..)
    , perform
    )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value)
import Script.Platform as Platform exposing (Platform)
import Task exposing (Task)


type Script x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script x a))
    | Invoke String Value (Decoder (Script x a))


perform : Task x a -> Script x a
perform =
    Task.map Succeed >> Task.onError (Fail >> Task.succeed) >> Perform


type FileSystem
    = FileSystem


type alias Path =
    List String


type Directory p
    = Directory Path


type File p
    = File Path


type EnvironmentVariables
    = EnvironmentVariables Platform (Dict String String)


type NetworkConnection
    = NetworkConnection


type Shell
    = Shell
