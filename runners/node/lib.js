"use strict";

let majorVersion = 4;
let minorVersion = 0;

let vm = require("vm");
let fs = require("fs");
let path = require("path");
let child_process = require("child_process");
let which = require("which");
let findUp = require("find-up");
let tmp = require("tmp");

function resolvePath(components) {
  if (components.length == 0) {
    throw { code: "ENOENT", message: "Empty path given" };
  }

  let result = path.normalize(components[0]);
  for (var i = 1; i < components.length; i++) {
    let childPath = path.resolve(result, components[i]);
    if (path.relative(result, childPath).startsWith("..")) {
      throw {
        code: "EACCES",
        message: components[i] + " is not a proper relative path"
      };
    }
    result = childPath;
  }
  return result;
}

function listEntities(request, responsePort, statsPredicate) {
  try {
    let directoryPath = resolvePath(request.value);
    let results = fs.readdirSync(directoryPath).filter(function(entity) {
      return statsPredicate(fs.statSync(path.resolve(directoryPath, entity)));
    });
    responsePort.send(results);
  } catch (error) {
    responsePort.send({ code: error.code, message: error.message });
  }
}

function runCompiledJs(absolutePath, commandLineArgs) {
  // Set up browser-like context in which to run compiled Elm code
  global.XMLHttpRequest = require("xhr2");
  global.setTimeout = require("timers").setTimeout;

  // Read compiled JS from file
  var compiledJs = null;
  try {
    compiledJs = fs.readFileSync(absolutePath, "utf8");
  } catch (error) {
    console.log(error.message);
    process.exit(1);
  }

  // Run Elm code to create the 'Elm' object
  vm.runInThisContext(compiledJs);

  // Create Elm worker and get its request/response ports
  let flags = {};
  flags["arguments"] = commandLineArgs;
  switch (process.platform) {
    case "aix":
    case "darwin":
    case "freebsd":
    case "linux":
    case "openbsd":
    case "sunos":
      flags["platform"] = "posix";
      break;
    case "win32":
      flags["platform"] = "windows";
      break;
    default:
      console.log("Unrecognized platform '" + process.platform + "'");
      process.exit(1);
  }
  flags["environmentVariables"] = Object.entries(process.env);
  let script = global["Elm"].Main.init({flags: flags});
  let requestPort = script.ports.requestPort;
  let responsePort = script.ports.responsePort;

  // Listen for requests, send responses when required
  requestPort.subscribe(function(request) {
    switch (request.name) {
      case "checkVersion":
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
              "Please update script to use a newer version of the ianmackenzie/script-experiment package"
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
      case "writeStdout":
        process.stdout.write(request.value);
        responsePort.send(null);
        break;
      case "exit":
        process.exit(request.value);
      case "readFile":
        try {
          let filePath = resolvePath(request.value);
          let contents = fs.readFileSync(filePath, "utf8");
          responsePort.send(contents);
        } catch (error) {
          responsePort.send({ code: error.code, message: error.message });
        }
        break;
      case "writeFile":
        try {
          let filePath = resolvePath(request.value.path);
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
          "Try updating to newer versions of elm-run and the ianmackenzie/script-experiment package"
        );
        process.exit(1);
    }
  });
}

module.exports = function(inputFileName, commandLineArgs) {
  let absolutePath = path.resolve(inputFileName);
  let directory = path.dirname(absolutePath);
  let extension = path.extname(absolutePath);
  if (extension === ".js") {
    runCompiledJs(absolutePath, commandLineArgs);
  } else if (extension === ".elm") {
    // Find path to corresponding elm.json
    let elmJsonPath = findUp.sync("elm.json", {cwd: directory});
    if (elmJsonPath == null) {
      console.log("Could not find elm.json in parent directory of " + absolutePath);
      process.exit(1);
    }
    let elmJsonDirectory = path.dirname(elmJsonPath);

    // Find Elm executable
    let cwd = process.cwd();
    // Switch to elm.json directory to find Elm executable
    process.chdir(elmJsonDirectory);
    let elmExecutable = which.sync("elm");
    // Switch back to original working directory
    process.chdir(cwd);
    if (elmExecutable == null) {
      console.log("Could not find Elm executable in " + elmJsonDirectory + " or PATH");
      process.exit(1);
    }

    // Create temporary JS file for Elm compiler output
    let outputJsFile = null;
    try {
      outputJsFile = tmp.fileSync({postfix: ".js"}).name;
    } catch (error) {
      console.log("Could not create temporary JavaScript file")
    }

    // Try to compile Elm file
    try {
      child_process.execFileSync(elmExecutable, ["make", "--optimize", "--output=" + outputJsFile, absolutePath], {cwd: elmJsonDirectory, encoding: "utf8"})
    } catch (error) {
      process.exit(1);
    }

    // Run the compiled JS
    runCompiledJs(outputJsFile, commandLineArgs);
  } else {
    console.log("Unrecognized source file extension " + extension + " (expecting .elm or .js");
    process.exit(1);
  }
};
