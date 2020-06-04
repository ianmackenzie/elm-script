## ianmackenzie/elm-script

**EXPERIMENTAL** - expect breaking changes, missing functionality, incomplete
documentation etc.

This package allows you define command-line scripts in Elm that can

  - Read and write files
  - Accept command-line arguments
  - Read environment variables
  - Make HTTP requests
  - Run subprocesses

Here's a complete "Hello World" program ([examples/HelloWorld.elm](https://github.com/ianmackenzie/elm-script/blob/master/examples/HelloWorld.elm)):

```elm
module HelloWorld exposing (main)

import Script exposing (Script)


script : Script.Init -> Script String ()
script {} =
    Script.printLine "Hello World!"


main : Script.Program
main =
    Script.program script
```

And here's a slightly more realistic/useful script that counts the number of
lines in files given at the command line ([examples/LineCounts.elm](https://github.com/ianmackenzie/elm-script/blob/master/examples/LineCounts.elm)):

```elm
module LineCounts exposing (main)

import Script exposing (Script)
import Script.File as File exposing (File, ReadOnly)


getLineCount : File ReadOnly -> Script String Int
getLineCount file =
    File.read file
        |> Script.map (String.trimRight >> String.lines >> List.length)


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    List.map (File.readOnly userPrivileges) arguments
        |> Script.collect getLineCount
        |> Script.map (List.map2 Tuple.pair arguments)
        |> Script.thenWith
            (Script.each
                (\( fileName, lineCount ) ->
                    Script.printLine
                        (fileName
                            ++ ": "
                            ++ String.fromInt lineCount
                            ++ " lines"
                        )
                )
            )


main : Script.Program
main =
    Script.program script
```

One of they key features of this package is very explicit control over
permissions. The top-level script has full access to the file system,
environment variables etc. but it can choose exactly how much access to give to
helper scripts. A `Script` cannot by default do anything other than a few
harmless things like getting the current time and printing to the console; in
order to do anything more, it must explicitly be given read access to a
particular directory, write access to a particular file, network access etc. In
the above example, you can know just from the type signature of `getLineCount`
that the returned script can read the file that you pass it, but it can't read
any other files, it can't write to any files at all, and it can't access the
network (to, say, send the contents of `passwords.txt` to an evil server
somewhere).

My hope is that this will make it possible to share scripting functionality via
the Elm package system without worrying that some script written by a stranger
is going to format your hard drive. Even when just writing your own scripts, the
type system helps keep track of which parts of your code are doing file I/O (to
what files, in what directories), which parts are performing network requests,
which parts are running potentially dangerous subprocesses, etc.

# Getting started

`ianmackenzie/elm-script` has not yet been published, so right now if you
want to play around with it you'll have to check out this repository. You can
then either just experiment with the files in the `examples` directory, or add
the `src` directory of this package to the `source-directories` field of your
`elm.json`.

To actually run scripts, you'll need to first install [Deno](https://deno.land/).
You should then be able to [install](https://deno.land/manual/tools/script_installer)
the `elm-script` command by running

```
deno install -A -n elm-script path/to/elm-script/runner/main.js
```

This will create a small executable file named `elm-script` that calls Deno to
execute [`runner/main.js`](https://github.com/ianmackenzie/elm-script/blob/master/runner/main.js).
Where exactly the file gets installed depends on your operating system and Deno
configuration, but you will need to make sure that directory gets added to your
PATH; see the [`deno install` docs](https://deno.land/manual/tools/script_installer)
for details. Once that is all done, you should be able to run Elm scripts using

```
elm-script run Main.elm
```

Refer to the API documentation for more details, or check out some more
[examples](examples).
