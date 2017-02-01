module Kintail.Script
    exposing
        ( Script
        , run
        , init
        , succeed
        , fail
        , map
        , map2
        , andThen
        , print
        , perform
        , attempt
        , sleep
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Task exposing (Task)
import Process
import Time exposing (Time)
import Html


type Script a
    = Run ( Cmd (Script a), Sub (Script a) )
    | Succeed a
    | Fail String


commands : Script a -> Cmd (Script a)
commands script =
    case script of
        Run ( commands, _ ) ->
            commands

        Succeed _ ->
            Cmd.none

        Fail _ ->
            Cmd.none


subscriptions : Script a -> Sub (Script a)
subscriptions script =
    case script of
        Run ( _, subscriptions ) ->
            subscriptions

        Succeed _ ->
            Sub.none

        Fail _ ->
            Sub.none


view script =
    case script of
        Run ( _, _ ) ->
            Html.text "Running..."

        Succeed value ->
            Html.text ("Succeeded: " ++ toString value)

        Fail error ->
            Html.text ("Failed: " ++ error)


run : Script a -> Program Never (Script a) (Script a)
run script =
    Html.program
        { init = ( script, commands script )
        , update = \updated _ -> ( updated, commands updated )
        , subscriptions = subscriptions
        , view = view
        }


init : a -> Script a
init =
    Succeed


succeed : a -> Script a
succeed =
    Succeed


fail : String -> Script a
fail =
    Fail


map : (a -> b) -> Script a -> Script b
map function script =
    case script of
        Run ( commands, subscriptions ) ->
            let
                mappedCommands =
                    Cmd.map (map function) commands

                mappedSubscriptions =
                    Sub.map (map function) subscriptions
            in
                Run ( mappedCommands, mappedSubscriptions )

        Succeed a ->
            Succeed (function a)

        Fail error ->
            Fail error


map2 : (a -> b -> c) -> Script a -> Script b -> Script c
map2 function scriptA scriptB =
    case ( scriptA, scriptB ) of
        ( Run ( commandsA, subscriptionsA ), _ ) ->
            let
                mapMessageA updatedScriptA =
                    map2 function updatedScriptA scriptB

                mappedCommands =
                    Cmd.map mapMessageA commandsA

                mappedSubscriptions =
                    Sub.map mapMessageA subscriptionsA
            in
                Run ( mappedCommands, mappedSubscriptions )

        ( Succeed valueA, Run ( commandsB, subscriptionsB ) ) ->
            let
                mapMessageB updatedScriptB =
                    map2 function scriptA updatedScriptB

                mappedCommands =
                    Cmd.map mapMessageB commandsB

                mappedSubscriptions =
                    Sub.map mapMessageB subscriptionsB
            in
                Run ( mappedCommands, mappedSubscriptions )

        ( Succeed valueA, Succeed valueB ) ->
            Succeed (function valueA valueB)

        ( Fail error, _ ) ->
            Fail error

        ( Succeed _, Fail error ) ->
            Fail error


andThen : (a -> Script b) -> Script a -> Script b
andThen function script =
    case script of
        Run ( commands, subscriptions ) ->
            let
                mappedCommands =
                    Cmd.map (andThen function) commands

                mappedSubscriptions =
                    Sub.map (andThen function) subscriptions
            in
                Run ( mappedCommands, mappedSubscriptions )

        Succeed value ->
            function value

        Fail error ->
            Fail error


print : (a -> b) -> Script a -> Script a
print selector =
    map
        (\value ->
            let
                _ =
                    Debug.log "output" (selector value)
            in
                value
        )


perform : Task Never a -> Script a
perform task =
    Run ( Task.perform Succeed task, Sub.none )


attempt : Task x a -> Script (Result x a)
attempt task =
    Run ( Task.attempt Succeed task, Sub.none )


sleep : (a -> Time) -> Script a -> Script a
sleep selector =
    andThen
        (\value ->
            perform (Process.sleep (selector value) |> Task.map (always value))
        )
