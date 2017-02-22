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
        , forEach
        , sequence
        , andThen
        , aside
        , map
        , ignore
        , map2
        , map3
        , map4
        , mapError
        , onError
        , retryUntilSuccess
        , perform
        , request
        , Arguments
        , with
        , andWith
        , yield
        , return
        , readFile
        , writeFile
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

@docs do, forEach, sequence, andThen, aside

# Mapping

@docs map, ignore, map2, map3, map4, mapError

# Combining

@docs with, andWith, yield, return

# Error recovery

@docs attempt, onError, retryUntilSuccess

# Tasks

@docs perform

# Requests

@docs request

# Files

@docs readFile, writeFile
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Task exposing (Task)
import Process
import Time exposing (Time)
import Http


type Script x a
    = Run ( (String -> Value -> Cmd Never) -> Cmd (Script x a), Value -> Script x a )
    | Succeed a
    | Fail x


type alias RequestPort =
    Value -> Cmd Never


type alias ResponsePort =
    (Value -> Value) -> Sub Value


type Msg x a
    = Updated (Script x a)
    | Response Value


run : Script Int () -> RequestPort -> ResponsePort -> Program Never (Script Int ()) (Msg Int ())
run script requestPort responsePort =
    let
        submitRequest name value =
            requestPort <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "value", value )
                    ]

        commands script =
            case script of
                Run ( buildCommands, _ ) ->
                    buildCommands submitRequest |> Cmd.map Updated

                Succeed () ->
                    submitRequest "succeed" Encode.null |> Cmd.map never

                Fail errorCode ->
                    submitRequest "fail" (Encode.int errorCode) |> Cmd.map never

        update message script =
            case message of
                Updated updated ->
                    ( updated, commands updated )

                Response value ->
                    case script of
                        Run ( _, responseHandler ) ->
                            let
                                updated =
                                    responseHandler value
                            in
                                ( updated, commands updated )

                        _ ->
                            Debug.crash "Received response from JavaScript with no script running"
    in
        Platform.program
            { init = ( script, commands script )
            , update = update
            , subscriptions =
                always (responsePort identity |> Sub.map Response)
            }


noResponseHandler : Value -> Script x a
noResponseHandler value =
    Debug.crash "Script has no response handler"


init : a -> Script x a
init =
    succeed


succeed : a -> Script x a
succeed =
    Succeed


fail : x -> Script x a
fail =
    Fail


map : (a -> b) -> Script x a -> Script x b
map function =
    andThen (function >> succeed)


map2 :
    (a -> b -> c)
    -> Script x a
    -> Script x b
    -> Script x c
map2 function scriptA scriptB =
    scriptA |> andThen (\valueA -> map (function valueA) scriptB)


map3 :
    (a -> b -> c -> d)
    -> Script x a
    -> Script x b
    -> Script x c
    -> Script x d
map3 function scriptA scriptB scriptC =
    scriptA |> andThen (\valueA -> map2 (function valueA) scriptB scriptC)


map4 :
    (a -> b -> c -> d -> e)
    -> Script x a
    -> Script x b
    -> Script x c
    -> Script x d
    -> Script x e
map4 function scriptA scriptB scriptC scriptD =
    scriptA |> andThen (\valueA -> map3 (function valueA) scriptB scriptC scriptD)


andThen : (a -> Script x b) -> Script x a -> Script x b
andThen function script =
    case script of
        Run ( buildCommands, responseHandler ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (andThen function)

                mappedResponseHandler =
                    responseHandler >> andThen function
            in
                Run ( buildMappedCommands, mappedResponseHandler )

        Succeed value ->
            function value

        Fail error ->
            fail error


ignore : Script x a -> Script x ()
ignore =
    map (always ())


do : List (Script x ()) -> Script x ()
do scripts =
    case scripts of
        [] ->
            succeed ()

        first :: rest ->
            first |> andThen (\() -> do rest)


forEach : List a -> (a -> Script x ()) -> Script x ()
forEach values function =
    do (List.map function values)


aside : (a -> Script x ()) -> Script x a -> Script x a
aside function =
    andThen (\value -> function value |> andThen (\() -> succeed value))


submitRequest : String -> Value -> Script x ()
submitRequest name value =
    let
        buildCommands submitRequest =
            Cmd.batch
                [ submitRequest name value |> Cmd.map never
                , Task.perform identity (Task.succeed (succeed ()))
                ]
    in
        Run ( buildCommands, noResponseHandler )


print : String -> Script x ()
print string =
    submitRequest "print" (Encode.string string)


perform : Task x a -> Script x a
perform task =
    let
        mapResult result =
            case result of
                Ok value ->
                    succeed value

                Err error ->
                    fail error
    in
        Run ( always (Task.attempt mapResult task), noResponseHandler )


request : Http.Request a -> Script Http.Error a
request =
    perform << Http.toTask


sleep : Time -> Script x ()
sleep time =
    perform (Process.sleep time)


onError : (x -> Script y a) -> Script x a -> Script y a
onError recover script =
    case script of
        Run ( buildCommands, responseHandler ) ->
            let
                buildMappedCommands =
                    buildCommands >> Cmd.map (onError recover)

                mappedResponseHandler =
                    responseHandler >> onError recover
            in
                Run ( buildMappedCommands, mappedResponseHandler )

        Succeed value ->
            succeed value

        Fail error ->
            recover error


mapError : (x -> y) -> Script x a -> Script y a
mapError function =
    onError (function >> fail)


attempt : Script x a -> Script y (Result x a)
attempt =
    map Ok >> onError (Err >> succeed)


retryUntilSuccess : Script x a -> Script y a
retryUntilSuccess script =
    onError (\error -> retryUntilSuccess script) script


sequence : List (Script x a) -> Script x (List a)
sequence scripts =
    case scripts of
        [] ->
            succeed []

        first :: rest ->
            first |> andThen (\value -> sequence rest |> map ((::) value))


type Arguments f r
    = Arguments (f -> r)


with : Script x a -> Script x (Arguments (a -> r) r)
with =
    map (\value -> Arguments (\function -> function value))


andWith : Script x b -> Script x (Arguments f (b -> r)) -> Script x (Arguments f r)
andWith scriptB argumentsScriptA =
    map2
        (\(Arguments callerA) valueB ->
            Arguments (\valueA -> callerA valueA valueB)
        )
        argumentsScriptA
        scriptB


yield : f -> Script x (Arguments f (Script x r)) -> Script x r
yield function =
    andThen (\(Arguments caller) -> caller function)


return : f -> Script x (Arguments f r) -> Script x r
return function =
    map (\(Arguments caller) -> caller function)


readFile : String -> Script String String
readFile =
    let
        responseDecoder =
            Decode.oneOf
                [ Decode.field "value" Decode.string |> Decode.map succeed
                , Decode.field "error" Decode.string |> Decode.map fail
                ]

        responseHandler value =
            case Decode.decodeValue responseDecoder value of
                Ok script ->
                    script

                Err message ->
                    Debug.crash "Unexpected JSON returned from JavaScript"
    in
        (\filename ->
            let
                buildCommands submitRequest =
                    submitRequest "readFile" (Encode.string filename)
                        |> Cmd.map never
            in
                Run ( buildCommands, responseHandler )
        )


writeFile : String -> String -> Script String ()
writeFile =
    let
        responseDecoder =
            Decode.oneOf
                [ Decode.null (succeed ())
                , Decode.field "error" Decode.string |> Decode.map fail
                ]

        responseHandler value =
            case Decode.decodeValue responseDecoder value of
                Ok script ->
                    script

                Err message ->
                    Debug.crash "Unexpected JSON returned from JavaScript"
    in
        (\filename contents ->
            let
                buildCommands submitRequest =
                    submitRequest "writeFile"
                        (Encode.object
                            [ ( "filename", Encode.string filename )
                            , ( "contents", Encode.string contents )
                            ]
                        )
                        |> Cmd.map never
            in
                Run ( buildCommands, responseHandler )
        )
