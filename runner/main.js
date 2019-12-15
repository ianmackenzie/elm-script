"use strict";

const majorVersion = 0;
const minorVersion = 1;

const majorProtocolVersion = 9;
const minorProtocolVersion = 1;

import * as path from "https://deno.land/std/path/mod.ts";

const tempDirectoriesToRemove = [];

function createTemporaryDirectory() {
  // Create a new temp directory
  const directoryPath = Deno.makeTempDirSync();
  // Add it to the list of temp directories to remove when the script has
  // finished executing
  tempDirectoriesToRemove.push(directoryPath);
  return directoryPath;
}

function exit(code) {
  // First, clean up any temp directories created while running the script
  for (const directoryPath of tempDirectoriesToRemove) {
    try {
      Deno.removeSync(directoryPath, { recursive: true });
    } catch (error) {
      // Ignore any errors that may occur when attempting to delete a
      // temporary directory - likely the directory was just deleted
      // explicitly, and even if it's some other issue (directory
      // somehow became read-only, in use because an antivirus program is
      // currently checking it etc.) it's not generally the end of the
      // world if the odd temp directory doesn't get deleted. (Script
      // authors who need to make sure sensitive data gets deleted can
      // always call Directory.obliterate in their script and check for
      // any errors resulting from it.)
      continue;
    }
  }
  // Finally, actually exit
  Deno.exit(code);
}

function resolvePath(components) {
  if (components.length == 0) {
    throw Error("Empty path given");
  }

  let result = path.resolve(components[0]);
  for (var i = 1; i < components.length; i++) {
    const childPath = path.resolve(result, components[i]);
    if (path.relative(result, childPath).startsWith("..")) {
      throw Error(components[i] + " is not a proper relative path");
    }
    result = childPath;
  }
  return result;
}

function listEntities(request, responsePort, statsPredicate) {
  try {
    const directoryPath = resolvePath(request.value);
    const results = Deno.readDirSync(directoryPath)
      .filter(function(fileInfo) {
        return statsPredicate(fileInfo);
      })
      .map(function(fileInfo) {
        return fileInfo.name;
      });
    responsePort.send(results);
  } catch (error) {
    responsePort.send({ message: error.message });
  }
}

// From https://github.com/github/fetch/issues/175#issuecomment-284787564
function timeout(ms, promise) {
  return new Promise(function(resolve, reject) {
    setTimeout(function() {
      reject(new Error("timeout"));
    }, ms);
    promise.then(resolve, reject);
  });
}

function runCompiledJs(jsFileName, commandLineArgs) {
  // Read compiled JS from file
  const jsData = Deno.readFileSync(jsFileName);
  const jsText = new TextDecoder("utf-8").decode(jsData);

  // Run Elm code to create the 'Elm' object
  const globalEval = eval;
  globalEval(jsText);

  // Create Elm worker and get its request/response ports
  const flags = {};
  flags["arguments"] = commandLineArgs;
  switch (Deno.build.os) {
    case "mac":
    case "linux":
      flags["platform"] = { type: "posix", name: Deno.build.os };
      break;
    case "win":
      flags["platform"] = { type: "windows" };
      break;
    default:
      console.log("Unrecognized OS '" + Deno.build.os + "'");
      exit(1);
  }
  flags["environment"] = Object.entries(Deno.env());
  flags["workingDirectory"] = Deno.cwd();
  const compiledPrograms = Object.values(globalThis["Elm"]);
  if (compiledPrograms.length != 1) {
    console.log(
      `Expected exactly 1 compiled program, found ${compiledPrograms.length}`
    );
    exit(1);
  }
  const program = compiledPrograms[0];
  const script = program.init({ flags: flags });
  const requestPort = script.ports.requestPort;
  const responsePort = script.ports.responsePort;

  // Listen for requests, send responses when required
  requestPort.subscribe(async function(request) {
    switch (request.name) {
      case "checkVersion":
        const requiredMajorProtocolVersion = request.value[0];
        const requiredMinorProtocolVersion = request.value[1];
        const describeCurrentProtocolVersion = ` (current elm-run protocol version: ${majorProtocolVersion}.${minorProtocolVersion})`;
        if (requiredMajorProtocolVersion !== majorProtocolVersion) {
          console.log(
            "Version mismatch: script requires elm-run major protocol version " +
              requiredMajorProtocolVersion +
              describeCurrentProtocolVersion
          );
          if (requiredMajorProtocolVersion > majorProtocolVersion) {
            console.log("Please update to a newer version of elm-run");
          } else {
            console.log(
              "Please update script to use a newer version of the ianmackenzie/elm-script package"
            );
          }
          exit(1);
        } else if (requiredMinorProtocolVersion > minorProtocolVersion) {
          const requiredProtocolVersionString =
            requiredMajorProtocolVersion + "." + requiredMinorProtocolVersion;
          console.log(
            "Version mismatch: script requires elm-run protocol version at least " +
              requiredProtocolVersionString +
              describeCurrentProtocolVersion
          );
          console.log("Please update to a newer version of elm-run");
          exit(1);
        } else {
          responsePort.send(null);
        }
        break;
      case "writeStdout":
        try {
          const data = new TextEncoder().encode(request.value);
          Deno.stdout.writeSync(data);
          responsePort.send(null);
        } catch (error) {
          console.log("Error printing to stdout");
          exit(1);
        }
        break;
      case "exit":
        exit(request.value);
      case "abort":
        const data = new TextEncoder().encode(request.value);
        Deno.stdout.writeSync(data);
        exit(1);
      case "readFile":
        try {
          const filePath = resolvePath(request.value);
          const data = Deno.readFileSync(filePath);
          const contents = new TextDecoder("utf-8").decode(data);
          responsePort.send(contents);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "writeFile":
        try {
          const filePath = resolvePath(request.value.path);
          const contents = new TextEncoder().encode(request.value.contents);
          Deno.writeFileSync(filePath, contents);
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "listFiles":
        listEntities(request, responsePort, fileInfo => fileInfo.isFile());
        break;
      case "listSubdirectories":
        listEntities(request, responsePort, fileInfo => fileInfo.isDirectory());
        break;
      case "execute":
        try {
          const process = Deno.run({
            args: [request.value.command, ...request.value.arguments],
            cwd: resolvePath(request.value.workingDirectory),
            stdout: "piped"
          });
          const result = await process.status();
          if (result.success) {
            const data = await process.output();
            const output = new TextDecoder("utf-8").decode(data);
            responsePort.send(output);
          } else {
            if (result.code !== null) {
              responsePort.send({ error: "exited", code: result.code });
            } else if (result.signal !== null) {
              responsePort.send({ error: "terminated" });
            } else {
              const data = await process.stderrOutput();
              const output = new TextDecoder("utf-8").decode(data);
              responsePort.send({ error: "failed", message: output });
            }
          }
        } catch (error) {
          if (error.kind == Deno.ErrorKind.NotFound) {
            responsePort.send({ error: "notfound" });
          } else {
            console.log(error.message);
            exit(1);
          }
        }
        break;
      case "copyFile":
        try {
          const sourcePath = resolvePath(request.value.sourcePath);
          const destinationPath = resolvePath(request.value.destinationPath);
          Deno.copyFileSync(sourcePath, destinationPath);
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "moveFile":
        try {
          const sourcePath = resolvePath(request.value.sourcePath);
          const destinationPath = resolvePath(request.value.destinationPath);
          Deno.renameSync(sourcePath, destinationPath);
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "deleteFile":
        try {
          const filePath = resolvePath(request.value);
          Deno.removeSync(filePath);
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "stat":
        try {
          const entityPath = resolvePath(request.value);
          const fileInfo = Deno.statSync(entityPath);
          if (fileInfo.isFile()) {
            responsePort.send("file");
          } else if (fileInfo.isDirectory()) {
            responsePort.send("directory");
          } else {
            responsePort.send("other");
          }
        } catch (error) {
          if (error.kind == Deno.ErrorKind.NotFound) {
            responsePort.send("nonexistent");
          } else {
            responsePort.send({ message: error.message });
          }
        }
        break;
      case "createDirectory":
        try {
          const directoryPath = resolvePath(request.value.path);
          Deno.mkdirSync(directoryPath, request.value.recursive);
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "removeDirectory":
        try {
          const directoryPath = resolvePath(request.value.path);
          Deno.removeSync(directoryPath, {
            recursive: request.value.recursive
          });
          responsePort.send(null);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "createTemporaryDirectory":
        try {
          const directoryPath = createTemporaryDirectory();
          responsePort.send(directoryPath);
        } catch (error) {
          responsePort.send({ message: error.message });
        }
        break;
      case "http":
        try {
          let promise = fetch(request.value.url, request.value.options);
          if (request.value.timeout != null) {
            promise = timeout(request.value.timeout, promise);
          }
          const httpResponse = await promise;
          const responseBody = await httpResponse.text();
          responsePort.send({
            status: httpResponse.status,
            body: responseBody
          });
        } catch (error) {
          let errorType = null;
          if (error.message == "timeout") {
            errorType = "Timeout";
          } else {
            errorType = "NetworkError";
          }
          responsePort.send({ error: errorType });
        }
        break;
      default:
        console.log(`Internal error - unexpected request ${request}`);
        console.log(
          "Try updating to newer versions of elm-run and the ianmackenzie/elm-script package"
        );
        exit(1);
    }
  });
}

async function main() {
  if (Deno.args.length >= 3) {
    const subcommand = Deno.args[1];
    if (subcommand !== "run") {
      console.log(`Run as 'elm-script run Script.elm [arguments]'`);
      exit(1);
    }
    const sourceFileName = Deno.args[2];
    const commandLineArgs = Deno.args.slice(3);
    const absolutePath = path.resolve(sourceFileName);
    const extension = path.extname(absolutePath);
    if (extension === ".js") {
      runCompiledJs(absolutePath, commandLineArgs);
    } else if (extension === ".elm") {
      const tempDirectory = createTemporaryDirectory();
      const tempFileName = path.resolve(tempDirectory, "main.js");
      const elmProcess = Deno.run({
        args: [
          "elm",
          "make",
          "--optimize",
          "--output=" + tempFileName,
          absolutePath
        ],
        stdout: "null"
      });
      const elmResult = await elmProcess.status();
      if (elmResult.success) {
        runCompiledJs(tempFileName, commandLineArgs);
      } else {
        // The Elm compiler will have printed out a compilation error
        // message, no need to add our own
        exit(1);
      }
    } else {
      console.log(
        `Unrecognized source file extension ${extension} (expecting.elm or.js)`
      );
      exit(1);
    }
  } else {
    console.log(`Run as 'elm-script run Script.elm [arguments]'`);
    exit(1);
  }
}

main();
