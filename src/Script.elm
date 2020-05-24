module Script exposing
    ( Script, Init, UserPrivileges, SubprocessError(..)
    , Program, program, customProgram
    , succeed, fail
    , printLine, sleep, getCurrentTime
    , executeWith, tryToExecuteWith
    , map, map2, map3, map4, ignoreResult
    , do, each, sequence, collect, andThen, thenWith, aside
    , mapError, attempt, onError, ignoreError, finally
    )

{-| The functions in this module let you define scripts, chain them together in
various ways, and turn them into runnable programs.

@docs Script, Init, UserPrivileges, SubprocessError


# Running

@docs RequestPort, ResponsePort, Program, program, customProgram


# Basics

@docs succeed, fail


# Utilities

@docs printLine, sleep, getCurrentTime


# Running external executables

@docs executeWith, tryToExecuteWith


# Mapping

@docs map, map2, map3, map4, ignoreResult


# Sequencing

@docs do, each, sequence, collect, andThen, thenWith, aside


# Error handling

@docs mapError, attempt, onError, perform, ignoreError, finally

-}

import Dict exposing (Dict)
import Duration exposing (Duration)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Platform.Cmd as Cmd
import Process
import Script.Directory exposing (Directory)
import Script.Environment exposing (Environment)
import Script.File exposing (File)
import Script.Http exposing (NetworkConnection)
import Script.Internal as Internal exposing (Flags)
import Script.Path as Path exposing (Path(..))
import Script.Permissions exposing (Writable)
import Script.Platform as Platform exposing (Platform(..))
import Task exposing (Task)
import Time


requiredProtocolVersion : ( Int, Int )
requiredProtocolVersion =
    ( 9, 1 )


{-| A `Script x a` value defines a script that, when run, will either produce a
value of type `a` or an error of type `x`.
-}
type alias Script x a =
    Internal.Script x a


type alias Init =
    { arguments : List String
    , workingDirectory : Directory Writable
    , platform : Platform
    , environment : Environment
    , networkConnection : NetworkConnection
    , userPrivileges : UserPrivileges
    }


type alias UserPrivileges =
    Internal.UserPrivileges


type SubprocessError
    = ExecutableNotFound
    | SubprocessFailed String
    | SubprocessWasTerminated
    | SubprocessExitedWithError Int


type Model
    = Running Flags (Script Int ())
    | Aborting


type Msg
    = Updated (Script Int ())
    | Response (Result Http.Error Value)


{-| The type of program returned by `Script.program`.
-}
type alias Program =
    Platform.Program Value Model Msg


decodeFlags : Decoder Flags
decodeFlags =
    Decode.field "platform" decodePlatform
        |> Decode.andThen
            (\platform ->
                Decode.map4 Flags
                    (Decode.field "arguments" (Decode.list Decode.string))
                    (Decode.succeed platform)
                    (Decode.field "environment" (decodeEnvironment platform))
                    (Decode.field "workingDirectory" (decodeWorkingDirectoryPath platform))
            )


decodePlatform : Decoder Platform
decodePlatform =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\platformType ->
                case platformType of
                    "posix" ->
                        Decode.map Posix (Decode.field "name" Decode.string)
                        
                    "darwin" ->
                        Decode.map Posix (Decode.field "name" Decode.string)

                    "windows" ->
                        Decode.succeed Windows

                    _ ->
                        Decode.fail ("Unrecognized platform type '" ++ platformType ++ "'")
            )


decodeKeyValuePair : Decoder ( String, String )
decodeKeyValuePair =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)


decodeEnvironment : Platform -> Decoder Environment
decodeEnvironment platform =
    Decode.list decodeKeyValuePair
        |> Decode.map
            (\keyValuePairs ->
                Internal.Environment platform
                    (Dict.fromList <|
                        -- On Windows, capitalize environment variable names
                        -- so they can be looked up case-insensitively (same
                        -- behavior as process.env in Node)
                        case platform of
                            Posix _ ->
                                keyValuePairs

                            Windows ->
                                List.map (Tuple.mapFirst String.toUpper) keyValuePairs
                    )
            )


decodeWorkingDirectoryPath : Platform -> Decoder Path
decodeWorkingDirectoryPath platform =
    Decode.string |> Decode.map (Path.absolute platform)


{-| Actually create a runnable script program! Your top-level script file should
have `main` defined as

    main : Script.Program
    main =
        Script.program script requestPort responsePort

The function provided as the first argument to `Script.program` must accept a
`Context` value and produce a `Script String ()`. If this script succeeds with
`()`, then a value of 0 will be returned to the operating system as the return
value of the script. If the script fails with an `String`, then that message
will be printed to the console and a value of 1 will be returned to the
operating system instead.

-}
program : (Init -> Script String ()) -> Program
program main =
    customProgram
        (\init ->
            main init
                |> onError (\message -> printLine message |> andThen (fail 1))
        )


{-| Like `program`, but with a bit more control: allows you to control the
integer error code returned to the operating system on failure, and does not
print out anything by default (you will have to print out any error messages
explicitly yourself).
-}
customProgram : (Init -> Script Int ()) -> Program
customProgram main =
    let
        checkProtocolVersion =
            let
                ( requiredMajorProtocolVersion, requiredMinorProtocolVersion ) =
                    requiredProtocolVersion

                encodedProtocolVersion =
                    Encode.list Encode.int
                        [ requiredMajorProtocolVersion, requiredMinorProtocolVersion ]
            in
            Internal.Invoke "checkVersion" encodedProtocolVersion <|
                \flags -> Decode.null (succeed ())

        init flagsValue =
            case Decode.decodeValue decodeFlags flagsValue of
                Ok decodedFlags ->
                    let
                        workingDirectory =
                            Internal.Directory decodedFlags.workingDirectoryPath

                        userPrivileges =
                            Internal.UserPrivileges decodedFlags.workingDirectoryPath

                        runMain =
                            main
                                { arguments = decodedFlags.arguments
                                , workingDirectory = workingDirectory
                                , platform = decodedFlags.platform
                                , environment = decodedFlags.environment
                                , networkConnection = Internal.NetworkConnection
                                , userPrivileges = userPrivileges
                                }

                        script =
                            checkProtocolVersion |> andThen runMain
                    in
                    ( Running decodedFlags script, commands script )

                Err _ ->
                    abort "Failed to decode flags from JavaScript"

        submitRequest name value =
            Http.post
                { url = "/runner"
                , body =
                    Http.jsonBody <|
                        Encode.object
                            [ ( "name", Encode.string name )
                            , ( "value", value )
                            ]
                , expect =
                    Http.expectJson Response Decode.value
                }

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

                Internal.Do command ->
                    Cmd.map Updated command

        abort message =
            ( Aborting, submitRequest "abort" (Encode.string message) )

        update message model =
            case model of
                Aborting ->
                    ( model, Cmd.none )

                Running flags current ->
                    case message of
                        Updated updated ->
                            ( Running flags updated, commands updated )

                        Response (Ok value) ->
                            case current of
                                Internal.Invoke _ _ decoder ->
                                    case Decode.decodeValue (decoder flags) value of
                                        Ok updated ->
                                            ( Running flags updated, commands updated )

                                        Err decodeError ->
                                            abort ("Failed to decode response from JavaScript: " ++ Decode.errorToString decodeError)

                                _ ->
                                    abort ("Received unexpected response from JavaScript: " ++ Encode.encode 0 value)

                        Response (Err _) ->
                            abort "Internal HTTP request failed"
    in
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
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
                Script.printLine ("Hello " ++ name ++ "!")

            [] ->
                Script.printLine "Please enter a name"
                    |> Script.andThen (Script.fail 1)

            _ ->
                Script.printLine "Please enter only one name!"
                    |> Script.andThen (Script.fail 2)

-}
fail : x -> Script x a
fail =
    Internal.Fail


{-| Print a line to the console. A newline will be added to the given string if
it does not already have one, so all of the following are equivalent:

    Script.do
        [ Script.printLine "Hello"
        , Script.printLine "World"
        ]

    Script.do
        [ Script.printLine "Hello\n"
        , Script.printLine "World\n"
        ]

    Script.printLine "Hello\nWorld"

    Script.printLine "Hello\nWorld\n"

-}
printLine : String -> Script x ()
printLine string =
    let
        stringWithNewline =
            if String.endsWith "\n" string then
                string

            else
                string ++ "\n"
    in
    Internal.Invoke "writeStdout" (Encode.string stringWithNewline) <|
        \flags -> Decode.null (succeed ())


{-| Sleep (pause) for the given number of milliseconds.

    -- Sleep for 5 seconds
    Script.sleep (Duration.milliseconds 5000)

-}
sleep : Duration -> Script x ()
sleep duration =
    Internal.perform (Process.sleep (Duration.inMilliseconds duration))


{-| Get the current time.

    Script.getCurrentTime
        |> Script.thenWith
            (\currentTime ->
                Script.printLine <|
                    "Number of hours since January 1, 1970: "
                        ++ toString (Time.inHours currentTime)
            )

-}
getCurrentTime : Script x Time.Posix
getCurrentTime =
    Internal.perform Time.now


perform : Script Never a -> Script x a
perform script =
    script |> onError never


{-| Map the value produced by a script; to get a list of lines from a file
instead of the entire contents as a single string, you might do

    getLines : Script File.Error (List String)
    getLines =
        File.read inputFile |> Script.map String.lines

-}
map : (a -> b) -> Script x a -> Script x b
map =
    Internal.map


{-| Map over the values produced by two scripts. The two scripts will be run in
sequence.
-}
map2 :
    (a -> b -> c)
    -> Script x a
    -> Script x b
    -> Script x c
map2 function scriptA scriptB =
    scriptA |> thenWith (\valueA -> map (function valueA) scriptB)


{-| Map over the values produced by three scripts. The three scripts will be run
in sequence.
-}
map3 :
    (a -> b -> c -> d)
    -> Script x a
    -> Script x b
    -> Script x c
    -> Script x d
map3 function scriptA scriptB scriptC =
    scriptA |> thenWith (\valueA -> map2 (function valueA) scriptB scriptC)


{-| Map over the values produced by four scripts. The four scripts will be run
in sequence.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Script x a
    -> Script x b
    -> Script x c
    -> Script x d
    -> Script x e
map4 function scriptA scriptB scriptC scriptD =
    scriptA |> thenWith (\valueA -> map3 (function valueA) scriptB scriptC scriptD)


{-| Explicitly ignore the value produced by a script. This is sometimes useful
when using a function like `Script.do` that expects all of its arguments to have
the type `Script x ()` (a script that produces no meaningful output):

    Script.do
        [ Script.printLine "Reading file..."
        , Script.readFile inputFile |> Script.ignoreResult
        , Script.printLine "Read file!"
        ]

(Why would you want to read a file without doing anything with the output,
though?)

-}
ignoreResult : Script x a -> Script x ()
ignoreResult =
    map (always ())


{-| Run a list of scripts in sequence. `Script.do` expects each given script to
have a return type of `()` (no meaningful output), and so itself has a return
type of `()`.

    Script.do
        [ Script.printLine "Reading a file..."
        , File.read inputFile
            |> Script.map String.lines
            |> Script.thenWith
                (\lines ->
                    Script.printLine <|
                        toString (List.length lines)
                            ++ " lines"
                )
        ]

If you need to run a list of scripts but collect their return values, use
`Script.sequence` instead.

-}
do : List (Script x ()) -> Script x ()
do scripts =
    case scripts of
        [] ->
            succeed ()

        first :: rest ->
            first |> andThen (do rest)


{-| For every value in a given list, call the given function and run the
script that it creates. From `examples/Each.elm`:

    script :
        List String
        -> Script.WorkingDirectory
        -> Script.Host
        -> Script.UserPrivileges
        -> Script Int ()
    script arguments host =
        arguments
            |> Script.each
                (\argument ->
                    Script.printLine <|
                        case String.toFloat argument of
                            Ok value ->
                                let
                                    squared =
                                        value * value
                                in
                                argument ++ " squared is " ++ toString squared

                            Err _ ->
                                argument ++ " is not a number!"
                )

Often works well with `Script.thenWith` if the previous script produces a list
of values:

    Directory.listFiles directory
        |> Script.thenWith
            (Script.each
                (\file ->
                    Script.printLine (File.name file)
                )
            )

-}
each : (a -> Script x ()) -> List a -> Script x ()
each function values =
    do (List.map function values)


{-| Run a list of scripts in sequence and collect their results into a list.
-}
sequence : List (Script x a) -> Script x (List a)
sequence scripts =
    case scripts of
        [] ->
            succeed []

        first :: rest ->
            first |> thenWith (\value -> map ((::) value) (sequence rest))


{-| For every value in a given list, call the given function and run the script
that it creates, then collect the results of all those scripts into a list.

    readAll : Script File.Error (List String)
    readAll =
        Script.collect File.read
            [ file1, file2, file3 ]

-}
collect : (a -> Script x b) -> List a -> Script x (List b)
collect function values =
    sequence (List.map function values)


{-| Take the output from one script and feed it into a second script:

    File.read inputFile
        |> Script.thenWith
            (\fileContents ->
                Script.printLine contents
            )

This is the most fundamental way to chain scripts together! Pretty much all
other combinators in this module (`each`, `do`, `map` etc.) can be implemented
in terms of `thenWith`, so if there's some custom behavior you need that is not
covered by one of those functions you should be able to implement it using
`thenWith`.

-}
thenWith : (a -> Script x b) -> Script x a -> Script x b
thenWith =
    Internal.thenWith


andThen : Script x a -> Script x () -> Script x a
andThen secondScript firstScript =
    firstScript |> thenWith (\() -> secondScript)


{-| Sometimes you can run into problems chaining scripts together using
`thenWith` if you want to do 'auxiliary' things like print to the console, log
to a file etc. For example, the following will **not** work:

    File.read inputFile
        |> Script.thenWith
            (\contents -> Script.print "OK, read file")
        |> Script.thenWith
            (\contents -> ...)

`File.read inputFile` succeeds with a `String` which is passed into the first
`thenWith`, but since `Script.print` succeeds with just the unit value `()` that
is what gets passed into the second `thenWith`!

You can use `aside` for this purpose, which lets you run a script on some
produced value but then 'pass it through' to the next script:

    File.read inputFile
        |> Script.aside
            (\contents -> Script.print "OK, read file")
        |> Script.thenWith
            (\contents ->
                ...
            )

This is safe because `aside` enforces that the first script produces `()` - that
is, it doesn't actually produce any useful output that you might want anway.

-}
aside : (a -> Script x ()) -> Script x a -> Script x a
aside doSomething script =
    -- Run the given script...
    script
        |> thenWith
            (\value ->
                -- ...as an 'aside' do something with the generated value
                -- (logging, printing to console etc)...
                doSomething value
                    -- ...and finally, succeed with the original generated value
                    -- (not the unit return value of the 'aside' script)
                    |> andThen (succeed value)
            )


mapError : (x -> y) -> Script x a -> Script y a
mapError =
    Internal.mapError


attempt : Script x a -> Script y (Result x a)
attempt =
    map Ok >> onError (Err >> succeed)


onError : (x -> Script y a) -> Script x a -> Script y a
onError =
    Internal.onError


ignoreError : Script x () -> Script y ()
ignoreError =
    onError (always (succeed ()))


finally : Script Never () -> Script x a -> Script x a
finally cleanup script =
    script
        |> thenWith (\result -> perform cleanup |> andThen (succeed result))
        |> onError (\error -> perform cleanup |> andThen (fail error))


executeWith :
    UserPrivileges
    -> { workingDirectory : Directory Writable, command : String, arguments : List String }
    -> Internal.Script String String
executeWith userPrivileges arguments =
    tryToExecuteWith userPrivileges arguments
        |> mapError
            (\error ->
                case error of
                    ExecutableNotFound ->
                        "Executable '" ++ arguments.command ++ "' not found"

                    SubprocessFailed message ->
                        message

                    SubprocessWasTerminated ->
                        "Subprocess '" ++ arguments.command ++ "' terminated"

                    SubprocessExitedWithError code ->
                        "Subprocess '"
                            ++ arguments.command
                            ++ "' exited with code "
                            ++ String.fromInt code
            )


tryToExecuteWith :
    UserPrivileges
    -> { workingDirectory : Directory Writable, command : String, arguments : List String }
    -> Internal.Script SubprocessError String
tryToExecuteWith userPrivileges { workingDirectory, command, arguments } =
    let
        (Internal.Directory workingDirectoryPath) =
            workingDirectory
    in
    Internal.Invoke "execute"
        (Encode.object
            [ ( "command", Encode.string command )
            , ( "arguments", Encode.list Encode.string arguments )
            , ( "workingDirectory", Path.encode workingDirectoryPath )
            ]
        )
        (\flags ->
            Decode.oneOf
                [ Decode.string |> Decode.map Internal.Succeed
                , Decode.field "error" Decode.string
                    |> Decode.andThen
                        (\error ->
                            case error of
                                "notfound" ->
                                    Decode.succeed ExecutableNotFound

                                "failed" ->
                                    Decode.field "message" Decode.string
                                        |> Decode.map SubprocessFailed

                                "terminated" ->
                                    Decode.succeed SubprocessWasTerminated

                                "exited" ->
                                    Decode.field "code" Decode.int
                                        |> Decode.map SubprocessExitedWithError

                                _ ->
                                    Decode.fail "Unexpected execution error type"
                        )
                    |> Decode.map Internal.Fail
                ]
        )
