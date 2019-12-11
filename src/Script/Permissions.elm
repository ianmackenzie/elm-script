module Script.Permissions exposing
    ( Allowed
    , Permissions
    , Read
    , ReadOnly
    , Writable
    , Write
    , WriteOnly
    , readOnly
    , writable
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


type alias Writable =
    { read : Allowed, write : Allowed }


readOnly : Permissions ReadOnly
readOnly =
    Permissions


writeOnly : Permissions WriteOnly
writeOnly =
    Permissions


writable : Permissions Writable
writable =
    Permissions
