"use strict";

let majorVersion = 2;
let minorVersion = 0;

let vm = require("vm");
let fs = require("fs");
let path = require("path");
let child_process = require("child_process");
let compileToString = require("node-elm-compiler").compileToString;

function listEntities(request, responsePort, statsPredicate) {
  try {
    let directoryPath = path.join(...request.value);
    let results = fs.readdirSync(directoryPath).filter(function(entity) {
      return statsPredicate(fs.statSync(path.resolve(directoryPath, entity)));
    });
    responsePort.send(results);
  } catch (error) {
    responsePort.send({ code: error.code, message: error.message });
  }
}

function runCompiledJs(compiledJs, commandLineArgs) {
  // Set up browser-like context in which to run compiled Elm code
  global.XMLHttpRequest = require("xhr2");
  global.setTimeout = require("timers").setTimeout;
  // Run Elm code to create the 'Elm' object
  vm.runInThisContext(compiledJs);

  // Create Elm worker and get its request/response ports
  let flags = {};
  flags["arguments"] = commandLineArgs;
  flags["platformString"] = process.platform;
  flags["environmentVariables"] = Object.entries(process.env);
  let script = global["Elm"].Main.worker(flags);
  let requestPort = script.ports.requestPort;
  let responsePort = script.ports.responsePort;

  // Listen for requests, send responses when required
  requestPort.subscribe(function(request) {
    switch (request.name) {
      case "requiredVersion":
        let requiredMajorVersion = request.value[0];
        let requiredMinorVersion = request.value[1];
        let describeCurrentVersion =
          " (current elm-run version: " +
          majorVersion +
          "." +
          minorVersion +
          ")";
        if (requiredMajorVersion !== majorVersion) {
          console.log(
            "Version mismatch: script requires elm-run major version " +
              requiredMajorVersion +
              describeCurrentVersion
          );
          if (requiredMajorVersion > majorVersion) {
            console.log("Please update to a newer version of elm-run");
          } else {
            console.log(
              "Please update script to use a newer version of the ianmackenzie/script package"
            );
          }
          process.exit(1);
        } else if (requiredMinorVersion > minorVersion) {
          let requiredVersionString =
            requiredMajorVersion + "." + requiredMinorVersion;
          console.log(
            "Version mismatch: script requires elm-run version at least " +
              requiredVersionString +
              describeCurrentVersion
          );
          console.log("Please update to a newer version of elm-run");
          process.exit(1);
        } else {
          responsePort.send(null);
        }
        break;
      case "print":
        console.log(request.value);
        responsePort.send(null);
        break;
      case "exit":
        process.exit(request.value);
      case "readFile":
        try {
          let filePath = path.join(...request.value);
          let contents = fs.readFileSync(filePath, "utf8");
          responsePort.send(contents);
        } catch (error) {
          responsePort.send({ code: error.code, message: error.message });
        }
        break;
      case "writeFile":
        try {
          let filePath = path.join(...request.value.path);
          let contents = request.value.contents;
          fs.writeFileSync(filePath, contents, "utf8");
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ code: error.code, message: error.message });
        }
        break;
      case "listFiles":
        listEntities(request, responsePort, stats => stats.isFile());
        break;
      case "listSubdirectories":
        listEntities(request, responsePort, stats => stats.isDirectory());
        break;
      case "execute":
        try {
          let options = { encoding: "utf8", maxBuffer: 1024 * 1024 * 1024 };
          let output = child_process.execSync(request.value, options);
          responsePort.send(output);
        } catch (error) {
          if (error.status !== null) {
            responsePort.send({ error: "exited", code: error.status });
          } else if (error.signal !== null) {
            responsePort.send({ error: "terminated" });
          } else {
            responsePort.send({ error: "failed", message: error.message });
          }
        }
        break;
      default:
        console.log("Internal error - unexpected request: " + request);
        console.log(
          "Try updating to newer versions of elm-run and the ianmackenzie/script package"
        );
        process.exit(1);
    }
  });
}

module.exports = function(elmFileName, commandLineArgs) {
  let absolutePath = path.resolve(elmFileName);
  let directory = path.dirname(absolutePath);
  let compileOptions = { yes: true, cwd: directory };
  compileToString(absolutePath, compileOptions)
    .then(function(compiledJs) {
      runCompiledJs(compiledJs, commandLineArgs);
    })
    .catch(function(error) {
      console.log(error.message);
    });
};
