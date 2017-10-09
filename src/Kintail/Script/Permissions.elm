module Kintail.Script.Permissions
    exposing
        ( Allowed(Allowed)
        , ReadOnly
        , ReadWrite
        , WriteOnly
        , readOnly
        , readWrite
        , writeOnly
        )


type Allowed
    = Allowed


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
