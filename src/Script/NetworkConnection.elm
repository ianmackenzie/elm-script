module Script.NetworkConnection
    exposing
        ( NetworkConnection
        , sendRequest
        )

import Http
import Script.Internal as Internal


type alias NetworkConnection =
    Internal.NetworkConnection


sendRequest : Http.Request a -> NetworkConnection -> Internal.Script Http.Error a
sendRequest request networkConnection =
    Internal.perform (Http.toTask request)
