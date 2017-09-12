module Kintail.Script
    exposing
        ( Arguments
        , FileError
        , ProcessError(..)
        , Program
        , RequestPort
        , ResponsePort
        , Script
        , andThen
        , andWith
        , aside
        , attempt
        , collect
        , do
        , execute
        , fail
        , forEach
        , getEnvironmentVariable
        , ignore
        , init
        , listFiles
        , listSubdirectories
        , map
        , map2
        , map3
        , map4
        , mapError
        , onError
        , perform
        , print
        , program
        , readFile
        , request
        , retryUntilSuccess
        , return
        , sequence
        , sleep
        , succeed
        , with
        , writeFile
        , yield
        )

{-| The functions in this module let you define scripts, chain them together in
various ways, and turn them into runnable programs.

@docs Script


# Running

@docs RequestPort, ResponsePort, Model, Msg, program


# Basics

@docs succeed, fail, init


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

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Task exposing (Task)
import Time exposing (Time)


requiredHostVersion : ( Int, Int )
requiredHostVersion =
    ( 1, 0 )


{-| A `Script x a` value defines a script that, when run, will either produce a
value of type `a` or an error of type `x`.
-}
type Script x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script x a))
    | Invoke String Value (Decoder (Script x a))


type alias RequestPort =
    Value -> Cmd Msg


type alias ResponsePort =
    (Value -> Msg) -> Sub Msg


type Model
    = Model (Script Int ())


type Msg
    = Updated (Script Int ())
    | Response Value


type alias Program =
    Platform.Program (List String) Model Msg


program : (List String -> Script Int ()) -> RequestPort -> ResponsePort -> Program
program main requestPort responsePort =
    let
        checkHostVersion =
            let
                ( major, minor ) =
                    requiredHostVersion

                encodedVersion =
                    Encode.list [ Encode.int major, Encode.int minor ]

                decoder =
                    Decode.null (succeed ())
            in
            Invoke "requiredVersion" encodedVersion decoder

        init args =
            let
                script =
                    checkHostVersion |> andThen (\() -> main args)
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


{-| A script that succeeds immediately with the given value.
-}
succeed : a -> Script x a
succeed =
    Succeed


{-| A script that fails immediately with the given value. The following script
greets someone by their name given by the first command-line argument, or prints
an error message and then returns an error code if no names or multiple names
are given:

    script : List String -> Script Int ()
    script args =
        case args of
            [ name ] ->
                Script.print ("Hello " ++ name ++ "!")

            [] ->
                Script.do
                    [ Script.print "Please enter a name"
                    , Script.fail 1
                    ]

            _ ->
                Script.do
                    [ Script.print "Please enter only one name!"
                    , Script.fail 2
                    ]

-}
fail : x -> Script x a
fail =
    Fail


{-| Synonym for `succeed` that reads better when used to 'kick off' a script:

    Script.init { a = 3, b = 4 }
        |> Script.map .a
    --> Script.succeed 3

-}
init : a -> Script x a
init =
    succeed


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
map function script =
    script |> andThen (\value -> succeed (function value))


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


type ProcessError
    = ProcessFailed String
    | ProcessWasTerminated
    | ProcessExitedWithError Int


execute : String -> List String -> Script ProcessError String
execute command arguments =
    Invoke "execute"
        (Encode.object
            [ ( "command", Encode.string command )
            , ( "arguments", Encode.list (List.map Encode.string arguments) )
            ]
        )
        (Decode.oneOf
            [ Decode.string |> Decode.map succeed
            , Decode.field "error" Decode.string
                |> Decode.andThen
                    (\error ->
                        case error of
                            "failed" ->
                                Decode.field "message" Decode.string
                                    |> Decode.map ProcessFailed

                            "terminated" ->
                                Decode.succeed ProcessWasTerminated

                            "exited" ->
                                Decode.field "code" Decode.int
                                    |> Decode.map ProcessExitedWithError

                            _ ->
                                Decode.fail "Unexpected execution error type"
                    )
                |> Decode.map fail
            ]
        )
