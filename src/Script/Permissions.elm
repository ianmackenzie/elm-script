module Script.Permissions
    exposing
        ( Allowed
        , Read
        , ReadOnly
        , ReadWrite
        , Write
        , WriteOnly
        , readOnly
        , readWrite
        , writeOnly
        )


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


readOnly : ReadOnly
readOnly =
    { read = Allowed }


writeOnly : WriteOnly
writeOnly =
    { write = Allowed }


readWrite : ReadWrite
readWrite =
    { read = Allowed, write = Allowed }
