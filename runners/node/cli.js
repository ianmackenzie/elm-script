#!/usr/bin/env node

'use strict'

let run = require('./lib.js')

if (process.argv.length >= 3) {
  let elmFileName = process.argv[2]
  run(elmFileName, process.argv.slice(3))
} else {
  console.log("Run as 'elm-run Script.elm [arguments]'")
}
