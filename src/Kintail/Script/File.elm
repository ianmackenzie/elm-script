module Kintail.Script.File
    exposing
        ( File
        , name
        )

import Kintail.Script.Internal as Internal
import Kintail.Script.Path as Path


type alias File p =
    Internal.File p


name : File p -> String
name (Internal.File path) =
    Path.name path
