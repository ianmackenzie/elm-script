module Script.FileSystem
    exposing
        ( FileSystem
        , directory
        , file
        )

import Script.Internal as Internal


type alias FileSystem =
    Internal.FileSystem


directory : String -> FileSystem -> Internal.Directory p
directory path fileSystem =
    Internal.Directory [ path ]


file : String -> FileSystem -> Internal.File p
file path fileSystem =
    Internal.File [ path ]
