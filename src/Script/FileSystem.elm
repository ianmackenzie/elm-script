module Script.FileSystem
    exposing
        ( FileSystem
        , directory
        , file
        )

import Script.Internal as Internal
import Script.Permissions exposing (Permissions)


type alias FileSystem =
    Internal.FileSystem


directory : Permissions p -> String -> FileSystem -> Internal.Directory p
directory permissions path fileSystem =
    Internal.Directory [ path ]


file : Permissions p -> String -> FileSystem -> Internal.File p
file permissions path fileSystem =
    Internal.File [ path ]
