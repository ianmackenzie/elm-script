module Script.NetworkConnection
    exposing
        ( NetworkConnection
        , sendRequest
        )

import Http
import Script.Internal as Internal
import Task


type alias NetworkConnection =
    Internal.NetworkConnection


sendRequest : Http.Request a -> NetworkConnection -> Internal.Script Http.Error a
sendRequest request networkConnection =
    Http.toTask request
        |> Task.map Internal.Succeed
        |> Task.onError (Internal.Fail >> Task.succeed)
        |> Internal.Perform
