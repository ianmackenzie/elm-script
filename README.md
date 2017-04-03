## kintail/script

This package allows you define command-line scripts in Elm that can read and
write files, accept command-line arguments, and perform any other tasks that are
possible in Elm such as making HTTP requests.

This package is currently experimental - expect breaking changes

# Getting started

Create a new Elm package and add `kintail/script` as a dependency. Add a
`Main.elm` file that looks like this:

```elm
port module Main exposing (..)

import Kintail.Script as Script
import Json.Encode exposing (Value)

script : List String -> Script Int ()
script arguments =
    Script.print "Hello World!"

port requestPort : Value -> Cmd msg

port responsePort : (Value -> msg) -> Sub msg

main =
    Script.program script requestPort responsePort
```

Compile this file with

```
elm make --output main.js Main.elm
```

To actually run the compiled script, you will need to install `elm-run`:

```
npm install -g @kintail/elm-run
```

You should then be able to run your script with

```
elm-run main.js
```

Refer to the API documentation for more details, or check out some more
[examples](examples).
