## ianmackenzie/script

This package allows you define command-line scripts in Elm that can read and
write files, accept command-line arguments, and perform any other tasks that are
possible in Elm such as making HTTP requests.

This package is currently experimental - expect breaking changes.

# Getting started

`ianmackenzie/script` has not yet been published, so right now if you want to
play around with it you'll have to check out this repository. You can then
either just experiment with the files in the `examples` directory, or add the
`src` directory of this package to the `source-directories` field in your
package's `elm-package.json`.

To create a new script, create a `Main.elm` file that looks like this:

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

(The top-level script must have the type `Script Int ()`, i.e. the script must
either fail with an integer error code or succeed with the unit value, but it
can be composed out of smaller scripts that produce any kinds of errors and
success values.) Compile this file with:

```
elm make --output main.js Main.elm
```

To actually run the compiled script, you will need the Node-based `elm-run`
runner script. This has also not yet been published, so in the `runners/node`
directory run `npm link` to use the current version. You should then be able to
run your script with:

```
elm-run Main.elm
```

Refer to the API documentation for more details, or check out some more
[examples](examples).
