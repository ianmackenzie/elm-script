module Script
    exposing
        ( Arguments
        , Context
        , FileError
        , Program
        , RequestPort
        , ResponsePort
        , Script
        , andThen
        , andWith
        , aside
        , attempt
        , call
        , collect
        , do
        , fail
        , forEach
        , getCurrentTime
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

@docs Script, Context


# Running

@docs RequestPort, ResponsePort, program


# Basics

@docs succeed, fail, init


# Utilities

@docs print, sleep, getCurrentTime


# Mapping

@docs map, map2, map3, map4, ignore


# Sequencing

@docs do, forEach, sequence, collect, andThen, aside, call


# Combining

@docs Arguments, with, andWith, yield, return


# Error handling

@docs mapError, attempt, onError


# Files

@docs FileError, readFile, writeFile, listFiles, listSubdirectories

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Script.Directory as Directory exposing (Directory)
import Script.EnvironmentVariables exposing (EnvironmentVariables)
import Script.File exposing (File)
import Script.FileSystem as FileSystem exposing (FileSystem)
import Script.Internal as Internal
import Script.NetworkConnection exposing (NetworkConnection)
import Script.Path exposing (Path)
import Script.Permissions exposing (Read, Write)
import Script.Platform as Platform exposing (Platform)
import Script.Shell exposing (Shell)
import Task exposing (Task)
import Time exposing (Time)


requiredHostVersion : ( Int, Int )
requiredHostVersion =
    ( 2, 0 )


{-| A `Script x a` value defines a script that, when run, will either produce a
value of type `a` or an error of type `x`.
-}
type alias Script x a =
    Internal.Script x a


type alias Context =
    { arguments : List String
    , environmentVariables : EnvironmentVariables
    , fileSystem : FileSystem
    , networkConnection : NetworkConnection
    , shell : Shell
    , platform : Platform
    }


type alias Flags =
    { arguments : List String
    , platformString : String
    , environmentVariables : List ( String, String )
    }


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
    Platform.Program Flags Model Msg


toPlatform : String -> Platform
toPlatform platformString =
    case platformString of
        "aix" ->
            Platform.Posix

        "darwin" ->
            Platform.Posix

        "freebsd" ->
            Platform.Posix

        "linux" ->
            Platform.Posix

        "openbsd" ->
            Platform.Posix

        "sunos" ->
            Platform.Posix

        "win32" ->
            Platform.Windows

        _ ->
            Debug.crash ("Unrecognized platform '" ++ platformString ++ "'")


program : (Context -> Script Int ()) -> RequestPort -> ResponsePort -> Program
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
            Internal.Invoke "requiredVersion" encodedVersion decoder

        init flags =
            let
                platform =
                    toPlatform flags.platformString

                environmentVariables =
                    Internal.EnvironmentVariables platform
                        (Dict.fromList <|
                            -- On Windows, capitalize environment variable names
                            -- so they can be looked up case-insensitively (same
                            -- behavior as process.env in Node)
                            case platform of
                                Platform.Posix ->
                                    flags.environmentVariables

                                Platform.Windows ->
                                    List.map (Tuple.mapFirst String.toUpper)
                                        flags.environmentVariables
                        )

                context =
                    { arguments = flags.arguments
                    , environmentVariables = environmentVariables
                    , platform = platform
                    , fileSystem = Internal.FileSystem
                    , networkConnection = Internal.NetworkConnection
                    , shell = Internal.Shell
                    }

                script =
                    checkHostVersion |> andThen (\() -> main context)
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
                Internal.Succeed () ->
                    submitRequest "exit" (Encode.int 0)

                Internal.Fail errorCode ->
                    submitRequest "exit" (Encode.int errorCode)

                Internal.Perform task ->
                    Task.perform Updated task

                Internal.Invoke name value _ ->
                    submitRequest name value

        update message (Model current) =
            case message of
                Updated updated ->
                    ( Model updated, commands updated )

                Response value ->
                    case current of
                        Internal.Invoke _ _ decoder ->
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
    Internal.Succeed


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
                Script.print "Please enter a name"
                    |> Script.andThen (\() -> Script.fail 1)

            _ ->
                Script.print "Please enter only one name!"
                    |> Script.andThen (\() -> Script.fail 2)

-}
fail : x -> Script x a
fail =
    Internal.Fail


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
    Internal.Invoke "print" (Encode.string string) (Decode.null (succeed ()))


sleep : Time -> Script x ()
sleep time =
    Internal.Perform (Task.map succeed (Process.sleep time))


getCurrentTime : Script x Time
getCurrentTime =
    perform Time.now


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
        Internal.Succeed value ->
            function value

        Internal.Fail error ->
            fail error

        Internal.Perform task ->
            Internal.Perform (Task.map (andThen function) task)

        Internal.Invoke name value decoder ->
            Internal.Invoke name value (Decode.map (andThen function) decoder)


aside : (a -> Script x ()) -> Script x a -> Script x a
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


call : (() -> Result x a) -> Script x a
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
        Internal.Succeed value ->
            succeed value

        Internal.Fail error ->
            recover error

        Internal.Perform task ->
            Internal.Perform (Task.map (onError recover) task)

        Internal.Invoke name value decoder ->
            Internal.Invoke name value (Decode.map (onError recover) decoder)


perform : Task x a -> Script x a
perform =
    Task.map succeed >> Task.onError (fail >> Task.succeed) >> Internal.Perform


type alias FileError =
    { code : String
    , message : String
    }


fileErrorDecoder : Decoder FileError
fileErrorDecoder =
    Decode.map2 FileError
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


encodePath : Path -> Value
encodePath path =
    Encode.list (List.map Encode.string path)


readFile : File (Read p) -> Script FileError String
readFile (Internal.File path) =
    Internal.Invoke "readFile"
        (encodePath path)
        (Decode.oneOf
            [ Decode.string |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


writeFile : File (Write p) -> String -> Script FileError ()
writeFile (Internal.File path) contents =
    Internal.Invoke "writeFile"
        (Encode.object
            [ ( "path", encodePath path )
            , ( "contents", Encode.string contents )
            ]
        )
        (Decode.oneOf
            [ Decode.null (succeed ())
            , fileErrorDecoder |> Decode.map fail
            ]
        )


listFiles : Directory (Read p) -> Script FileError (List (File (Read p)))
listFiles ((Internal.Directory path) as directory) =
    Internal.Invoke "listFiles"
        (encodePath path)
        (Decode.oneOf
            [ Decode.list Decode.string
                |> Decode.map (List.map (\name -> Directory.file name directory))
                |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )


listSubdirectories : Directory (Read p) -> Script FileError (List (Directory (Read p)))
listSubdirectories ((Internal.Directory path) as directory) =
    Internal.Invoke "listSubdirectories"
        (encodePath path)
        (Decode.oneOf
            [ Decode.list Decode.string
                |> Decode.map (List.map (\name -> Directory.subdirectory name directory))
                |> Decode.map succeed
            , fileErrorDecoder |> Decode.map fail
            ]
        )
