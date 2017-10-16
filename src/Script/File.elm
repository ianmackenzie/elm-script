module Script.File
    exposing
        ( File
        , name
        )

import Script.Internal as Internal
import Script.Path as Path


type alias File p =
    Internal.File p


name : File p -> String
name (Internal.File path) =
    Path.name path
