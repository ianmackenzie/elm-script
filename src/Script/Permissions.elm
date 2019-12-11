module Script.Permissions exposing
    ( Allowed
    , Permissions
    , Read
    , ReadOnly
    , ReadWrite
    , Write
    , WriteOnly
    , readOnly
    , readWrite
    , writeOnly
    )


type Permissions p
    = Permissions


type Allowed
    = Allowed


type alias Read p =
    { p | read : Allowed }


type alias Write p =
    { p | write : Allowed }


type alias ReadOnly =
    { read : Allowed }


type alias WriteOnly =
    { write : Allowed }


type alias ReadWrite =
    { read : Allowed, write : Allowed }


readOnly : Permissions ReadOnly
readOnly =
    Permissions


writeOnly : Permissions WriteOnly
writeOnly =
    Permissions


readWrite : Permissions ReadWrite
readWrite =
    Permissions
