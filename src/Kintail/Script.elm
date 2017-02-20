module Kintail.Script
    exposing
        ( Script
        , init
        , succeed
        , fail
        , RequestPort
        , ResponsePort
        , run
        , print
        , sleep
        , do
        , sequence
        , andThen
        , andThenWith
        , aside
        , asideWith
        , repeatUntil
        , map
        , ignore
        , map2
        , map3
        , map4
        , onError
        , retryUntilSuccess
        , perform
        , attempt
        , Arguments
        , collect
        , andCollect
        , andThenWithCollected
        , mapCollected
        )

{-| The functions in this module let you define scripts, chain them together in
various ways, and turn them into runnable programs.

# Basics

@docs Script, init, succeed, fail

# Running

@docs RequestPort, ResponsePort, run

# Utilities

@docs print, sleep

# Sequencing

@docs do, sequence, andThen, andThenWith, aside, asideWith

# Repetition

@docs repeatUntil

# Mapping

@docs map, ignore, map2, map3, map4

# Combining

@docs collect, andCollect, andThenWithCollected, mapCollected

# Error recovery

@docs onError, retryUntilSuccess

# Tasks

@docs perform, attempt
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Task exposing (Task)
import Process
import Time exposing (Time)


type alias Context =
    { submitRequest : String -> Value -> Cmd Never
    , responses : Sub Value
    }


type Script a
    = Run ( Context -> Cmd (Script a), Context -> Sub (Script a) )
    | Succeed a
    | Fail String


type alias RequestPort =
    Value -> Cmd Never


type alias ResponsePort =
    (Value -> Value) -> Sub Value


commands : Context -> Script a -> Cmd (Script a)
commands context script =
    case script of
        Run ( buildCommands, _ ) ->
            buildCommands context

        Succeed _ ->
            context.submitRequest "succeed" Encode.null
                |> Cmd.map never

        Fail message ->
            context.submitRequest "fail" (Encode.string message)
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


run : Script a -> RequestPort -> ResponsePort -> Program Never (Script a) (Script a)
run script requestPort responsePort =
    let
        submitRequest name value =
            requestPort <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "value", value )
                    ]

        context =
            { submitRequest = submitRequest
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
    succeed


succeed : a -> Script a
succeed =
    Succeed


fail : String -> Script a
fail =
    Fail


map : (a -> b) -> Script a -> Script b
map function script =
    script |> andThenWith (\value -> succeed (function value))


map2 :
    (a -> b -> c)
    -> Script a
    -> Script b
    -> Script c
map2 function scriptA scriptB =
    scriptA |> andThenWith (\valueA -> map (function valueA) scriptB)


map3 :
    (a -> b -> c -> d)
    -> Script a
    -> Script b
    -> Script c
    -> Script d
map3 function scriptA scriptB scriptC =
    scriptA |> andThenWith (\valueA -> map2 (function valueA) scriptB scriptC)


map4 :
    (a -> b -> c -> d -> e)
    -> Script a
    -> Script b
    -> Script c
    -> Script d
    -> Script e
map4 function scriptA scriptB scriptC scriptD =
    scriptA
        |> andThenWith
            (\valueA -> map3 (function valueA) scriptB scriptC scriptD)


andThenWith : (a -> Script b) -> Script a -> Script b
andThenWith function script =
    case script of
        Run ( buildCommands, buildSubscriptions ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (andThenWith function)

                buildMappedSubscriptions =
                    buildSubscriptions >> Sub.map (andThenWith function)
            in
                Run ( buildMappedCommands, buildMappedSubscriptions )

        Succeed value ->
            function value

        Fail error ->
            fail error


andThen : Script a -> Script () -> Script a
andThen =
    andThenWith << always


ignore : Script a -> Script ()
ignore =
    map (always ())


do : List (Script ()) -> Script ()
do scripts =
    case scripts of
        [] ->
            succeed ()

        first :: rest ->
            first |> andThen (do rest)


asideWith : (a -> Script ()) -> Script a -> Script a
asideWith function =
    andThenWith (\value -> function value |> andThen (succeed value))


submitRequest : String -> Value -> Script ()
submitRequest name value =
    let
        buildCommands context =
            Cmd.batch
                [ context.submitRequest name value |> Cmd.map never
                , Task.perform identity (Task.succeed (succeed ()))
                ]
    in
        Run ( buildCommands, always Sub.none )


print : a -> Script ()
print value =
    submitRequest "print" (value |> toString |> Encode.string)


perform : Task Never a -> Script a
perform task =
    Run ( always (Task.perform succeed task), always Sub.none )


attempt : Task x a -> Script (Result x a)
attempt task =
    Run ( always (Task.attempt succeed task), always Sub.none )


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
            succeed value

        Fail _ ->
            fallback


repeatUntil : (a -> Bool) -> Script a -> Script a
repeatUntil predicate script =
    script
        |> andThenWith
            (\value ->
                if predicate value then
                    succeed value
                else
                    repeatUntil predicate script
            )


retryUntilSuccess : Script a -> Script a
retryUntilSuccess script =
    onError script script


sequence : List (Script a) -> Script (List a)
sequence scripts =
    case scripts of
        [] ->
            succeed []

        first :: rest ->
            first |> andThenWith (\value -> sequence rest |> map ((::) value))


aside : Script () -> Script a -> Script a
aside =
    asideWith << always


type Arguments f r
    = Arguments (f -> r)


collect : Script a -> Script (Arguments (a -> r) r)
collect =
    map (\value -> Arguments (\function -> function value))


andCollect : Script b -> Script (Arguments f (b -> r)) -> Script (Arguments f r)
andCollect scriptB argumentsScriptA =
    map2
        (\(Arguments callerA) valueB ->
            Arguments (\valueA -> callerA valueA valueB)
        )
        argumentsScriptA
        scriptB


andThenWithCollected : f -> Script (Arguments f (Script r)) -> Script r
andThenWithCollected function =
    andThenWith (\(Arguments caller) -> caller function)


mapCollected : f -> Script (Arguments f r) -> Script r
mapCollected function =
    map (\(Arguments caller) -> caller function)
