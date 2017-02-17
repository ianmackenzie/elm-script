module Kintail.Script
    exposing
        ( Script
        , run
        , init
        , succeed
        , fail
        , do
        , ignore
        , map
        , map2
        , andThen
        , with
        , print
        , sleep
        , perform
        , attempt
        , onError
        , retryUntil
        , retryUntilSuccess
        , sequence
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Task exposing (Task)
import Process
import Time exposing (Time)
import Html


type alias Context =
    { submitRequest : Value -> Cmd Never
    , responses : Sub Value
    }


type Script a
    = Run ( Context -> Cmd (Script a), Context -> Sub (Script a) )
    | Succeed a
    | Fail String


buildRequest : String -> Value -> Value
buildRequest name value =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "value", value )
        ]


commands : Context -> Script a -> Cmd (Script a)
commands context script =
    case script of
        Run ( buildCommands, _ ) ->
            buildCommands context

        Succeed _ ->
            context.submitRequest (buildRequest "succeed" Encode.null)
                |> Cmd.map never

        Fail message ->
            context.submitRequest (buildRequest "fail" (Encode.string message))
                |> Cmd.map never


subscriptions : Context -> Script a -> Sub (Script a)
subscriptions context script =
    case script of
        Run ( _, buildSubscriptions ) ->
            buildSubscriptions context

        Succeed _ ->
            Sub.none

        Fail _ ->
            Sub.none


run :
    Script a
    -> (Value -> Cmd Never)
    -> ((Value -> Value) -> Sub Value)
    -> Program Never (Script a) (Script a)
run script requestPort responsePort =
    let
        context =
            { submitRequest = requestPort
            , responses = responsePort identity
            }
    in
        Platform.program
            { init = ( script, commands context script )
            , update = \updated _ -> ( updated, commands context updated )
            , subscriptions = subscriptions context
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
        Run ( buildCommands, buildSubscriptions ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (map function)

                buildMappedSubscriptions =
                    buildSubscriptions >> Sub.map (map function)
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        Succeed a ->
            Succeed (function a)

        Fail error ->
            Fail error


map2 : (a -> b -> c) -> Script a -> Script b -> Script c
map2 function scriptA scriptB =
    case ( scriptA, scriptB ) of
        ( Run ( buildCommandsA, buildSubscriptionsA ), _ ) ->
            let
                mapMessageA updatedScriptA =
                    map2 function updatedScriptA scriptB

                buildMappedCommands =
                    buildCommandsA >> Cmd.map mapMessageA

                buildMappedSubscriptions =
                    buildSubscriptionsA >> Sub.map mapMessageA
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        ( Succeed valueA, Run ( buildCommandsB, buildSubscriptionsB ) ) ->
            let
                mapMessageB updatedScriptB =
                    map2 function scriptA updatedScriptB

                buildMappedCommands =
                    buildCommandsB >> Cmd.map mapMessageB

                buildMappedSubscriptions =
                    buildSubscriptionsB >> Sub.map mapMessageB
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        ( Succeed valueA, Succeed valueB ) ->
            Succeed (function valueA valueB)

        ( Fail error, _ ) ->
            Fail error

        ( Succeed _, Fail error ) ->
            Fail error


andThen : (a -> Script b) -> Script a -> Script b
andThen function script =
    case script of
        Run ( buildCommands, buildSubscriptions ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (andThen function)

                buildMappedSubscriptions =
                    buildSubscriptions >> Sub.map (andThen function)
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        Succeed value ->
            function value

        Fail error ->
            Fail error


ignore : Script a -> Script ()
ignore =
    map (always ())


do : List (Script ()) -> Script ()
do scripts =
    case scripts of
        [] ->
            Succeed ()

        first :: rest ->
            first |> andThen (always (do rest))


with : (a -> Script ()) -> Script a -> Script a
with function =
    andThen (\value -> function value |> andThen (always (Succeed value)))


submitRequest : String -> Value -> Script ()
submitRequest name value =
    let
        requestObject =
            buildRequest name value

        buildCommands context =
            Cmd.batch
                [ context.submitRequest requestObject |> Cmd.map never
                , Task.perform identity (Task.succeed (Succeed ()))
                ]
    in
        Run ( buildCommands, always Sub.none )


print : a -> Script ()
print value =
    submitRequest "print" (value |> toString |> Encode.string)


perform : Task Never a -> Script a
perform task =
    Run ( always (Task.perform Succeed task), always Sub.none )


attempt : Task x a -> Script (Result x a)
attempt task =
    Run ( always (Task.attempt Succeed task), always Sub.none )


sleep : Time -> Script ()
sleep time =
    perform (Process.sleep time)


onError : Script a -> Script a -> Script a
onError fallback script =
    case script of
        Run ( buildCommands, buildSubscriptions ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (onError fallback)

                buildMappedSubscriptions =
                    buildSubscriptions >> Sub.map (onError fallback)
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        Succeed value ->
            Succeed value

        Fail _ ->
            fallback


retryUntil : (a -> Bool) -> Script a -> Script a
retryUntil predicate script =
    script
        |> andThen
            (\value ->
                if predicate value then
                    succeed value
                else
                    retryUntil predicate script
            )


retryUntilSuccess : Script a -> Script a
retryUntilSuccess script =
    onError script script


sequence : List (Script a) -> Script (List a)
sequence scripts =
    case scripts of
        [] ->
            Succeed []

        first :: rest ->
            first |> andThen (\value -> sequence rest |> map ((::) value))
