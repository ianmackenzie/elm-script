module Kintail.Script
    exposing
        ( Script
        , init
        , succeed
        , fail
        , RequestPort
        , ResponsePort
        , program
        , print
        , sleep
        , getEnvironmentVariable
        , map
        , map2
        , map3
        , map4
        , ignore
        , do
        , forEach
        , sequence
        , collect
        , andThen
        , aside
        , Arguments
        , with
        , andWith
        , yield
        , return
        , mapError
        , attempt
        , onError
        , retryUntilSuccess
        , perform
        , request
        , FileError
        , readFile
        , writeFile
        , listFiles
        , listSubdirectories
        )

{-| The functions in this module let you define scripts, chain them together in
various ways, and turn them into runnable programs.

# Basics

@docs Script, init, succeed, fail

# Running

@docs RequestPort, ResponsePort, Model, Msg, program

# Utilities

@docs print, sleep, getEnvironmentVariable

# Mapping

@docs map, map2, map3, map4, ignore

# Sequencing

@docs do, forEach, sequence, collect, andThen, aside

# Combining

@docs Arguments, with, andWith, yield, return

# Error handling

@docs mapError, attempt, onError, retryUntilSuccess

# Tasks

@docs perform

# Requests

@docs request

# Files

@docs FileError, readFile, writeFile, listFiles, listSubdirectories
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)
import Process
import Time exposing (Time)
import Http


type Script x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script x a))
    | Invoke String Value (Decoder (Script x a))


init : a -> Script x a
init =
    succeed


succeed : a -> Script x a
succeed =
    Succeed


fail : x -> Script x a
fail =
    Fail


type alias RequestPort =
    Value -> Cmd Msg


type alias ResponsePort =
    (Value -> Msg) -> Sub Msg


type Model
    = Model (Script Int ())


type Msg
    = Updated (Script Int ())
    | Response Value


program : (List String -> Script Int ()) -> RequestPort -> ResponsePort -> Program (List String) Model Msg
program main requestPort responsePort =
    let
        init args =
            let
                script =
                    main args
            in
                ( Model script, commands script )

        submitRequest name value =
            requestPort <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "value", value )
                    ]

        commands script =
            case script of
                Succeed () ->
                    submitRequest "exit" (Encode.int 0)

                Fail errorCode ->
                    submitRequest "exit" (Encode.int errorCode)

                Perform task ->
                    Task.perform Updated task

                Invoke name value _ ->
                    submitRequest name value

        update message (Model current) =
            case message of
                Updated updated ->
                    ( Model updated, commands updated )

                Response value ->
                    case current of
                        Invoke _ _ decoder ->
                            case Decode.decodeValue decoder value of
                                Ok updated ->
                                    ( Model updated, commands updated )

                                Err message ->
                                    Debug.crash ("Failed to decode response from JavaScript: " ++ message)

                        _ ->
                            Debug.crash ("Received unexpected response from JavaScript: " ++ toString value)
    in
        Platform.programWithFlags
            { init = init
            , update = update
            , subscriptions = always (responsePort Response)
            }


print : String -> Script x ()
print string =
    Invoke "print" (Encode.string string) (Decode.null (succeed ()))


sleep : Time -> Script x ()
sleep time =
    perform (Process.sleep time)


getEnvironmentVariable : String -> Script x (Maybe String)
getEnvironmentVariable name =
    Invoke "getEnvironmentVariable"
        (Encode.string name)
        (Decode.nullable Decode.string |> Decode.map succeed)


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


forEach : (a -> Script x ()) -> List a -> Script x ()
forEach function values =
    do (List.map function values)


sequence : List (Script x a) -> Script x (List a)
sequence scripts =
    case scripts of
        [] ->
            succeed []

        first :: rest ->
            first |> andThen (\value -> sequence rest |> map ((::) value))


collect : (a -> Script x b) -> List a -> Script x (List b)
collect function values =
    sequence (List.map function values)


andThen : (a -> Script x b) -> Script x a -> Script x b
andThen function script =
    case script of
        Succeed value ->
            function value

        Fail error ->
            fail error

        Perform task ->
            Perform (Task.map (andThen function) task)

        Invoke name value decoder ->
            Invoke name value (Decode.map (andThen function) decoder)


aside : (a -> Script x ()) -> Script x a -> Script x a
aside function =
    andThen (\value -> function value |> andThen (\() -> succeed value))


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


mapError : (x -> y) -> Script x a -> Script y a
mapError function =
    onError (function >> fail)


attempt : Script x a -> Script y (Result x a)
attempt =
    map Ok >> onError (Err >> succeed)


onError : (x -> Script y a) -> Script x a -> Script y a
onError recover script =
    case script of
        Succeed value ->
            succeed value

        Fail error ->
            recover error

        Perform task ->
            Perform (Task.map (onError recover) task)

        Invoke name value decoder ->
            Invoke name value (Decode.map (onError recover) decoder)


retryUntilSuccess : Script x a -> Script y a
retryUntilSuccess script =
    onError (\error -> retryUntilSuccess script) script


perform : Task x a -> Script x a
perform =
    Task.map succeed >> Task.onError (fail >> Task.succeed) >> Perform


request : Http.Request a -> Script Http.Error a
request =
    perform << Http.toTask


type alias FileError =
    { code : String
    , message : String
    }


fileErrorDecoder : Decoder FileError
fileErrorDecoder =
    Decode.map2 FileError
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


readFile : String -> Script FileError String
readFile filename =
    Invoke "readFile"
        (Encode.string filename)
        (Decode.oneOf
            [ Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


writeFile : String -> String -> Script FileError ()
writeFile filename contents =
    Invoke "writeFile"
        (Encode.object
            [ ( "filename", Encode.string filename )
            , ( "contents", Encode.string contents )
            ]
        )
        (Decode.oneOf
            [ Decode.null (succeed ())
            , fileErrorDecoder |> Decode.map fail
            ]
        )


listFiles : String -> Script FileError (List String)
listFiles directory =
    Invoke "listFiles"
        (Encode.string directory)
        (Decode.oneOf
            [ Decode.list Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


listSubdirectories : String -> Script FileError (List String)
listSubdirectories directory =
    Invoke "listSubdirectories"
        (Encode.string directory)
        (Decode.oneOf
            [ Decode.list Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )
