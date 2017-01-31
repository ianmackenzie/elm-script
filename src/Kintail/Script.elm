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
    = Succeed a
    | Fail String
    | Run ( Cmd (Script a), Sub (Script a) )


commands : Script a -> Cmd (Script a)
commands script =
    case script of
        Run ( commands, _ ) ->
            commands

        _ ->
            Cmd.none


subscriptions : Script a -> Sub (Script a)
subscriptions script =
    case script of
        Run ( _, subscriptions ) ->
            subscriptions

        _ ->
            Sub.none


view script =
    case script of
        Succeed value ->
            Html.text ("Succeeded: " ++ toString value)

        Fail error ->
            Html.text ("Failed: " ++ error)

        Run ( _, _ ) ->
            Html.text "Running..."


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
        Succeed a ->
            succeed (function a)

        Fail error ->
            fail error

        Run ( commands, subscriptions ) ->
            let
                mappedCommands =
                    Cmd.map (map function) commands

                mappedSubscriptions =
                    Sub.map (map function) subscriptions
            in
                Run ( mappedCommands, mappedSubscriptions )


map2 : (a -> b -> c) -> Script a -> Script b -> Script c
map2 function scriptA scriptB =
    case ( scriptA, scriptB ) of
        ( Fail error, _ ) ->
            fail error

        ( _, Fail error ) ->
            fail error

        ( Succeed valueA, Succeed valueB ) ->
            Succeed (function valueA valueB)

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


andThen : (a -> Script b) -> Script a -> Script b
andThen function script =
    case script of
        Succeed value ->
            function value

        Fail error ->
            fail error

        Run ( commands, subscriptions ) ->
            let
                mappedCommands =
                    Cmd.map (andThen function) commands

                mappedSubscriptions =
                    Sub.map (andThen function) subscriptions
            in
                Run ( mappedCommands, mappedSubscriptions )


print : (a -> b) -> a -> Script a
print selector value =
    let
        output =
            selector value

        task =
            Task.succeed output
                |> Task.map (Debug.log "output")
                |> Task.map (always value)
    in
        perform task


perform : Task Never a -> Script a
perform task =
    Run ( Task.perform succeed task, Sub.none )


attempt : Task String a -> Script a
attempt task =
    let
        toScript result =
            case result of
                Ok value ->
                    succeed value

                Err error ->
                    fail error
    in
        Run ( Task.attempt toScript task, Sub.none )


sleep : Time -> a -> Script a
sleep time result =
    perform (Process.sleep time) |> andThen (always (succeed result))
