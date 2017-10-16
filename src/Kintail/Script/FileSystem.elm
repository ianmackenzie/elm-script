module Kintail.Script.FileSystem
    exposing
        ( FileSystem
        , directory
        , file
        , pathSeparator
        )

import Kintail.Script.Directory as Directory exposing (Directory)
import Kintail.Script.File as File exposing (File)
import Kintail.Script.Internal as Internal
import Kintail.Script.Path as Path
import Kintail.Script.Platform as Platform exposing (Platform)


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
