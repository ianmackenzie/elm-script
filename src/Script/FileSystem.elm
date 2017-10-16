module Script.FileSystem
    exposing
        ( FileSystem
        , directory
        , file
        , pathSeparator
        )

import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.Internal as Internal
import Script.Path as Path
import Script.Platform as Platform exposing (Platform)


type alias FileSystem =
    Internal.FileSystem


pathSeparator : Platform -> String
pathSeparator platform =
    Path.separator platform


directory : p -> String -> FileSystem -> Directory p
directory permissions path fileSystem =
    Internal.Directory [ path ]


file : p -> String -> FileSystem -> File p
file permissions path fileSystem =
    Internal.File [ path ]
