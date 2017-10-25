module Script.FileSystem
    exposing
        ( FileSystem
        , directory
        , file
        )

import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.Internal as Internal


type alias FileSystem =
    Internal.FileSystem


directory : p -> String -> FileSystem -> Directory p
directory permissions path fileSystem =
    Internal.Directory [ path ]


file : p -> String -> FileSystem -> File p
file permissions path fileSystem =
    Internal.File [ path ]
