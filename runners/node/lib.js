'use strict'

let vm = require('vm')
let fs = require('fs')
let path = require('path')

function listEntities (request, responsePort, statsPredicate) {
  try {
    let directory = request.value
    let results = fs.readdirSync(directory).filter(function (entity) {
      return statsPredicate(fs.statSync(path.resolve(directory, entity)))
    })
    responsePort.send(results)
  } catch (error) {
    responsePort.send({code: error.code, message: error.message})
  }
}

module.exports = function (path, args) {
  // Load compiled Elm code
  let source = fs.readFileSync(path)
  // Set up browser-like context in which to run compiled Elm code
  global.XMLHttpRequest = require('xhr2')
  global.setTimeout = require('timers').setTimeout
  // Run Elm code to create the 'Elm' object
  vm.runInThisContext(source, path)

  // Create Elm worker and get its request/response ports
  let script = global['Elm'].Main.worker(args)
  let requestPort = script.ports.requestPort
  let responsePort = script.ports.responsePort

  // Listen for requests, send responses when required
  requestPort.subscribe(function (request) {
    switch (request.name) {
      case 'print':
        console.log(request.value)
        responsePort.send(null)
        break
      case 'exit':
        process.exit(request.value)
        break
      case 'readFile':
        try {
          let filename = request.value
          let contents = fs.readFileSync(filename, 'utf8')
          responsePort.send(contents)
        } catch (error) {
          responsePort.send({code: error.code, message: error.message})
        }
        break
      case 'writeFile':
        try {
          let filename = request.value.filename
          let contents = request.value.contents
          fs.writeFileSync(filename, contents, 'utf8')
          responsePort.send(null)
        } catch (error) {
          responsePort.send({code: error.code, message: error.message})
        }
        break
      case 'getEnvironmentVariable':
        responsePort.send(process.env[request.value] || null)
        break
      case 'listFiles':
        listEntities(request, responsePort, stats => stats.isFile())
        break
      case 'listSubdirectories':
        listEntities(request, responsePort, stats => stats.isDirectory())
        break
    }
  })
}
