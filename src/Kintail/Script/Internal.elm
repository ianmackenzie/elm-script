module Kintail.Script.Internal exposing (..)


type Process
    = Process Flags


type alias Flags =
    { arguments : List String
    , workingDirectory : String
    }


type FileSystem
    = FileSystem


type alias Path =
    List String


type Directory p
    = Directory Path


type File p
    = File Path


type EnvironmentVariables p
    = EnvironmentVariables


type NetworkAccess
    = NetworkAccess


type Shell
    = Shell
