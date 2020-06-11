module Test.NestedModule exposing (main)

import Script exposing (Script)


script : Script.Init -> Script String ()
script _ =
    Script.printLine "Nested module works!"


main : Script.Program
main =
    Script.program script
