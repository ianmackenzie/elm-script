## ianmackenzie/script-experiment

**EXPERIMENTAL** - expect breaking changes.

This package allows you define command-line scripts in Elm that can

  - Read and write files
  - Accept command-line arguments
  - Read environment variables
  - Make HTTP requests
  - Run subprocesses

One of they key features of this package is very explicit control over
permissions - the top-level script has full access to all of the above
functionality, but it can choose exactly how much access to give to helper
scripts. A `Script` cannot by default do anything other than a few harmless
things like getting the current time and printing to the console; in order to do
anything more, it must explicitly be given read access to a particular
directory, write access to a particular file, network access etc. So if you see
a function like

```elm
countLines : Directory (Read p) -> Script Error Int
```

then you know that the returned script can read files within the directory that
you pass it (the `Directory (Read p)` type should be read as "directory with
read permissions"), but it can't read any files outside of that directory, it
can't write to any files at all, and it can't access the network (to, say, send
the contents of `passwords.txt` to a nefarious server somewhere).

My hope is that this will make it possible to share scripting functionality via
the Elm package system without worrying that some script written by a stranger
is going to format your hard drive. Even when just writing your own scripts, the
type system helps keep track of which parts of your code are doing file I/O (to
what files, in what directories), which parts are performing network requests,
which parts are running potentially dangerous subprocesses, etc.

# Getting started

`ianmackenzie/script-experiment` has not yet been published, so right now if you
want to play around with it you'll have to check out this repository. You can
then either just experiment with the files in the `examples` directory, or add
the `src` directory of this package to the `source-directories` field in your
package's `elm-package.json`.

To create a new script, create a `Main.elm` file that looks like this:

```elm
port module Main exposing (..)

import Script exposing (Script)
import Json.Encode exposing (Value)

script : Script.Context -> Script Int ()
script context =
    Script.printLine "Hello World!"

port requestPort : Value -> Cmd msg

port responsePort : (Value -> msg) -> Sub msg

main =
    Script.program script requestPort responsePort
```

(The top-level script must have the type `Script Int ()`, i.e. the script must
either fail with an integer error code or succeed with the unit value, but it
can be composed out of smaller scripts that produce any kinds of errors and
success values.)

To actually run the compiled script, you will need the Node-based `elm-run`
runner script. This has also not yet been published, so in the `runners/node`
directory run `npm link` to use the current version. You should then be able to
run your script with:

```
elm-run Main.elm
```

Refer to the API documentation for more details, or check out some more
[examples](examples).
