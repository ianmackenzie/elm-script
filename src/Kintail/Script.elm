module Kintail.Script
    exposing
        ( Allowed
        , Arguments
        , FileError
        , ProcessError(..)
        , Program
        , ReadOnly
        , RequestPort
        , ResponsePort
        , Safe
        , Sandboxed
        , Script
        , andThen
        , andWith
        , aside
        , attempt
        , call
        , collect
        , do
        , execute
        , fail
        , forEach
        , getCurrentTime
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
        , print
        , program
        , readFile
        , request
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


# Permissions

@docs Allowed, Safe, Sandboxed, ReadOnly


# Running

@docs RequestPort, ResponsePort, Model, Msg, program


# Basics

@docs succeed, fail, init


# Utilities

@docs print, sleep, getEnvironmentVariable, getCurrentTime


# Mapping

@docs map, map2, map3, map4, ignore


# Sequencing

@docs do, forEach, sequence, collect, andThen, aside, call


# Combining

@docs Arguments, with, andWith, yield, return


# Error handling

@docs mapError, attempt, onError


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
type Script p x a
    = Succeed a
    | Fail x
    | Perform (Task Never (Script p x a))
    | Invoke String Value (Decoder (Script p x a))


type Allowed
    = NotConstructable Allowed


type alias Safe =
    {}


type alias Sandboxed =
    { http : Allowed
    }


type alias ReadOnly =
    { http : Allowed
    , read : Allowed
    }


type alias RequestPort =
    Value -> Cmd Msg


type alias ResponsePort =
    (Value -> Msg) -> Sub Msg


type Model
    = Model (Script {} Int ())


type Msg
    = Updated (Script {} Int ())
    | Response Value


type alias Program =
    Platform.Program (List String) Model Msg


program : (List String -> Script p Int ()) -> RequestPort -> ResponsePort -> Program
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
                    checkHostVersion
                        |> andThen (\() -> changePermissions (main args))
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
succeed : a -> Script p x a
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
fail : x -> Script p x a
fail =
    Fail


{-| Synonym for `succeed` that reads better when used to 'kick off' a script:

    Script.init { a = 3, b = 4 }
        |> Script.map .a
    --> Script.succeed 3

-}
init : a -> Script p x a
init =
    succeed


print : String -> Script p x ()
print string =
    Invoke "print" (Encode.string string) (Decode.null (succeed ()))


sleep : Time -> Script p x ()
sleep time =
    Perform (Task.map succeed (Process.sleep time))


getEnvironmentVariable : String -> Script { p | read : Allowed } x (Maybe String)
getEnvironmentVariable name =
    Invoke "getEnvironmentVariable"
        (Encode.string name)
        (Decode.nullable Decode.string |> Decode.map succeed)


getCurrentTime : Script p x Time
getCurrentTime =
    perform Time.now


map : (a -> b) -> Script p x a -> Script p x b
map function script =
    script |> andThen (\value -> succeed (function value))


map2 :
    (a -> b -> c)
    -> Script p x a
    -> Script p x b
    -> Script p x c
map2 function scriptA scriptB =
    scriptA |> andThen (\valueA -> map (function valueA) scriptB)


map3 :
    (a -> b -> c -> d)
    -> Script p x a
    -> Script p x b
    -> Script p x c
    -> Script p x d
map3 function scriptA scriptB scriptC =
    scriptA |> andThen (\valueA -> map2 (function valueA) scriptB scriptC)


map4 :
    (a -> b -> c -> d -> e)
    -> Script p x a
    -> Script p x b
    -> Script p x c
    -> Script p x d
    -> Script p x e
map4 function scriptA scriptB scriptC scriptD =
    scriptA |> andThen (\valueA -> map3 (function valueA) scriptB scriptC scriptD)


ignore : Script p x a -> Script p x ()
ignore =
    map (always ())


do : List (Script p x ()) -> Script p x ()
do scripts =
    case scripts of
        [] ->
            succeed ()

        first :: rest ->
            first |> andThen (\() -> do rest)


forEach : (a -> Script p x ()) -> List a -> Script p x ()
forEach function values =
    do (List.map function values)


sequence : List (Script p x a) -> Script p x (List a)
sequence scripts =
    case scripts of
        [] ->
            succeed []

        first :: rest ->
            first |> andThen (\value -> sequence rest |> map ((::) value))


collect : (a -> Script p x b) -> List a -> Script p x (List b)
collect function values =
    sequence (List.map function values)


andThen : (a -> Script p x b) -> Script p x a -> Script p x b
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


aside : (a -> Script p x ()) -> Script p x a -> Script p x a
aside doSomethingWith script =
    -- Run the given script...
    script
        |> andThen
            (\value ->
                -- ...as an 'aside' do something with the generated value
                -- (logging, printing to console etc)...
                doSomethingWith value
                    |> andThen
                        -- ...finally, return the original generated value
                        -- (not the unit return value of the 'aside' script)
                        (\() -> succeed value)
            )


call : (() -> Result x a) -> Script p x a
call function =
    perform (Task.succeed ())
        |> andThen
            (\() ->
                case function () of
                    Ok result ->
                        succeed result

                    Err error ->
                        fail error
            )


type Arguments f r
    = Arguments (f -> r)


with : Script p x a -> Script p x (Arguments (a -> r) r)
with =
    map (\value -> Arguments (\function -> function value))


andWith : Script p x b -> Script p x (Arguments f (b -> r)) -> Script p x (Arguments f r)
andWith scriptB argumentsScriptA =
    map2
        (\(Arguments callerA) valueB ->
            Arguments (\valueA -> callerA valueA valueB)
        )
        argumentsScriptA
        scriptB


yield : f -> Script p x (Arguments f (Script p x r)) -> Script p x r
yield function =
    andThen (\(Arguments caller) -> caller function)


return : f -> Script p x (Arguments f r) -> Script p x r
return function =
    map (\(Arguments caller) -> caller function)


mapError : (x -> y) -> Script p x a -> Script p y a
mapError function =
    onError (function >> fail)


attempt : Script p x a -> Script p y (Result x a)
attempt =
    map Ok >> onError (Err >> succeed)


onError : (x -> Script p y a) -> Script p x a -> Script p y a
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


changePermissions : Script p1 x a -> Script p2 x a
changePermissions script =
    case script of
        Succeed value ->
            Succeed value

        Fail error ->
            Fail error

        Perform task ->
            Perform (Task.map changePermissions task)

        Invoke name value decoder ->
            Invoke name value (Decode.map changePermissions decoder)


perform : Task x a -> Script p x a
perform =
    Task.map succeed >> Task.onError (fail >> Task.succeed) >> Perform


request : Http.Request a -> Script { p | http : Allowed } Http.Error a
request =
    Http.toTask >> perform >> changePermissions


type alias FileError =
    { code : String
    , message : String
    }


fileErrorDecoder : Decoder FileError
fileErrorDecoder =
    Decode.map2 FileError
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


readFile : String -> Script { p | read : Allowed } FileError String
readFile filename =
    Invoke "readFile"
        (Encode.string filename)
        (Decode.oneOf
            [ Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


writeFile : String -> String -> Script { p | write : Allowed } FileError ()
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


listFiles : String -> Script { p | read : Allowed } FileError (List String)
listFiles directory =
    Invoke "listFiles"
        (Encode.string directory)
        (Decode.oneOf
            [ Decode.list Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


listSubdirectories : String -> Script { p | read : Allowed } FileError (List String)
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


execute : String -> List String -> Script { p | subprocesses : Allowed } ProcessError String
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
