#!/usr/bin/env node

"use strict";

let run = require("./lib.js");

if (process.argv.length >= 3) {
  let sourceFileName = process.argv[2];
  run(sourceFileName, process.argv.slice(3));
} else {
  console.log("Run as 'elm-run Script.elm [arguments]'");
}
