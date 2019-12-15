module Script.Internal exposing
    ( Directory(..)
    , Environment(..)
    , File(..)
    , Flags
    , NetworkConnection(..)
    , Script(..)
    , UserPrivileges(..)
    , map
    , mapError
    , onError
    , perform
    , thenWith
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
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


thenWith : (a -> Script x b) -> Script x a -> Script x b
thenWith function script =
    case script of
        Succeed value ->
            function value

        Fail error ->
            Fail error

        Perform task ->
            Perform (Task.map (thenWith function) task)

        Invoke name value decoder ->
            Invoke name value <|
                \flags -> Decode.map (thenWith function) (decoder flags)

        Do command ->
            Do (Cmd.map (thenWith function) command)


map : (a -> b) -> Script x a -> Script x b
map function script =
    script |> thenWith (\value -> Succeed (function value))


onError : (x -> Script y a) -> Script x a -> Script y a
onError recover script =
    case script of
        Succeed value ->
            Succeed value

        Fail error ->
            recover error

        Perform task ->
            Perform (Task.map (onError recover) task)

        Invoke name value decoder ->
            Invoke name value <|
                \flags -> Decode.map (onError recover) (decoder flags)

        Do command ->
            Do (Cmd.map (onError recover) command)


mapError : (x -> y) -> Script x a -> Script y a
mapError function =
    onError (function >> Fail)


perform : Task x a -> Script x a
perform task =
    Perform (Task.map Succeed task |> Task.onError (Fail >> Task.succeed))
