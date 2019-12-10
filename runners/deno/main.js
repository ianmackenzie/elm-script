"use strict";

const majorVersion = 5;
const minorVersion = 0;

import * as path from "https://deno.land/std/path/mod.ts";
import * as fs from "https://deno.land/std/fs/mod.ts";

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
        const results = Deno.readDirSync(directoryPath).filter(function (fileInfo) {
            return statsPredicate(fileInfo);
        }).map(function (fileInfo) {
            return fileInfo.name;
        });
        responsePort.send(results);
    } catch (error) {
        responsePort.send({ message: error.message });
    }
}

function runCompiledJs(compiledJs, commandLineArgs) {
    // Run Elm code to create the 'Elm' object
    const globalEval = eval;
    globalEval(compiledJs);

    // Create Elm worker and get its request/response ports
    const flags = {};
    flags["arguments"] = commandLineArgs;
    switch (Deno.build.os) {
        case "mac":
        case "linux":
            flags["platform"] = "posix";
            break;
        case "win":
            flags["platform"] = "windows";
            break;
        default:
            console.log("Unrecognized OS '" + Deno.build.os + "'");
            Deno.exit(1);
    }
    flags["environmentVariables"] = Object.entries(Deno.env());
    const compiledPrograms = Object.values(globalThis["Elm"]);
    if (compiledPrograms.length != 1) {
        console.log(`Expected exactly 1 compiled program, found ${compiledPrograms.length}`);
        Deno.exit(1);
    }
    const program = compiledPrograms[0];
    const script = program.init({ flags: flags });
    const requestPort = script.ports.requestPort;
    const responsePort = script.ports.responsePort;

    // Listen for requests, send responses when required
    requestPort.subscribe(async function (request) {
        switch (request.name) {
            case "checkVersion":
                const requiredMajorVersion = request.value[0];
                const requiredMinorVersion = request.value[1];
                const describeCurrentVersion =
                    ` (current elm-run version: ${majorVersion}.${minorVersion})`;
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
                    Deno.exit(1);
                } else if (requiredMinorVersion > minorVersion) {
                    const requiredVersionString =
                        requiredMajorVersion + "." + requiredMinorVersion;
                    console.log(
                        "Version mismatch: script requires elm-run version at least " +
                        requiredVersionString +
                        describeCurrentVersion
                    );
                    console.log("Please update to a newer version of elm-run");
                    Deno.exit(1);
                } else {
                    responsePort.send(null);
                }
                break;
            case "writeStdout":
                const data = new TextEncoder("utf-8").encode(request.value);
                Deno.stdout.writeSync(data);
                responsePort.send(null);
                break;
            case "exit":
                Deno.exit(request.value);
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
                    const contents = new TextEncoder("utf-8").encode(request.value.contents);
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
                let options = { args: [request.value.command] + request.value.arguments }
                if (request.value.workingDirectory) {
                    options.cwd = resolvePath(request.value.workingDirectory);
                }
                const process = Deno.run(options);
                const result = await process.status();
                if (result.success) {
                    const data = await process.output;
                    const output = new TextDecoder("utf-8").decode(data);
                    responsePort.send(output);
                } else {
                    if (result.code !== null) {
                        responsePort.send({ error: "exited", code: result.code });
                    } else if (result.signal !== null) {
                        responsePort.send({ error: "terminated" });
                    } else {
                        const data = await process.stderrOutput;
                        const output = new TextDecoder("utf-8").decode(data);
                        responsePort.send({ error: "failed", message: output });
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
                        responsePort.send("nonexistent");
                    }
                } catch (error) {
                    responsePort.send({ message: error.message });
                }
                break;
            case "createDirectory":
                try {
                    const directoryPath = resolvePath(request.value);
                    Deno.mkdirSync(directoryPath, true);
                    responsePort.send(null);
                } catch (error) {
                    responsePort.send({ message: error.message });
                }
                break;
            case "removeDirectory":
                try {
                    const directoryPath = resolvePath(request.value);
                    Deno.removeSync(directoryPath, { recursive: false });
                    responsePort.send(null);
                } catch (error) {
                    responsePort.send({ message: error.message });
                }
                break;
            case "obliterateDirectory":
                try {
                    const directoryPath = resolvePath(request.value);
                    Deno.removeSync(directoryPath, { recursive: true });
                    responsePort.send(null);
                } catch (error) {
                    responsePort.send({ message: error.message });
                }
                break;
            case "createTemporaryDirectory":
                try {
                    const directoryPath = Deno.makeTempDirSync();
                    responsePort.send(directoryPath);
                } catch (error) {
                    responsePort.send({ message: error.message });
                }
                break;
            default:
                console.log(`Internal error - unexpected request ${request}`);
                console.log("Try updating to newer versions of elm-run and the ianmackenzie/script-experiment package");
                Deno.exit(1);
        }
    });
}

if (Deno.args.length >= 2) {
    const sourceFileName = Deno.args[1];
    const commandLineArgs = Deno.args.slice(2);
    const absolutePath = path.resolve(sourceFileName);
    const extension = path.extname(absolutePath);
    if (extension === ".js") {
        // Read compiled JS from file
        const data = Deno.readFileSync(absolutePath);
        const compiledJs = new TextDecoder("utf-8").decode(data);
        runCompiledJs(compiledJs, commandLineArgs);
    } else if (extension === ".elm") {
        const tempDirectory = Deno.makeTempDirSync();
        const tempFileName = path.resolve(tempDirectory, "main.js");
        const elmProcess = Deno.run({
            args: ["elm", "make", "--optimize", "--output=" + tempFileName, absolutePath]
        });
        const elmResult = await elmProcess.status();
        if (elmResult.success) {
            const data = Deno.readFileSync(tempFileName);
            const compiledJs = new TextDecoder("utf-8").decode(data);
            console.log(`Would remove: ${tempDirectory}`);
            //Deno.removeSync(tempDirectory, { recursive: True });
            runCompiledJs(compiledJs, commandLineArgs);
        } else {
            Deno.exit(1);
        }
    } else {
        console.log(`Unrecognized source file extension ${extension} (expecting.elm or.js)`);
        Deno.exit(1);
    }
} else {
    console.log(`Run as 'deno elm-run.io/${majorVersion}.${minorVersion} Script.elm [arguments]'`);
}
