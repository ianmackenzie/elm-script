module HelloWorld exposing (main)

import Script exposing (Script)


script : Script.Init -> Script String ()
script {} =
    Script.printLine "Hello World!"


main : Script.Program
main =
    Script.program script
