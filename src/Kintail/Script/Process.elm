module Kintail.Script.Process
    exposing
        ( EnvironmentVariables
        , NetworkAccess
        , Process
        , Shell
        , arguments
        , environmentVariables
        , fileSystem
        , networkAccess
        , shell
        )

import Kintail.Script.Directory as Directory exposing (Directory)
import Kintail.Script.FileSystem as FileSystem exposing (FileSystem)
import Kintail.Script.Internal as Internal


type alias Process =
    Internal.Process


type alias EnvironmentVariables p =
    Internal.EnvironmentVariables p


type alias NetworkAccess =
    Internal.NetworkAccess


type alias Shell =
    Internal.Shell


arguments : Process -> List String
arguments (Internal.Process { arguments }) =
    arguments


workingDirectory : p -> Process -> Directory p
workingDirectory permissions (Internal.Process { workingDirectory }) =
    Internal.Directory [ workingDirectory ]


fileSystem : Process -> FileSystem
fileSystem process =
    Internal.FileSystem


environmentVariables : p -> Process -> EnvironmentVariables p
environmentVariables permissions process =
    Internal.EnvironmentVariables


networkAccess : Process -> NetworkAccess
networkAccess process =
    Internal.NetworkAccess


shell : Process -> Shell
shell process =
    Internal.Shell
