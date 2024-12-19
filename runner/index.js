"use strict";

const { execFile } = require("child_process");
const { existsSync } = require("fs");
const fs = require("fs");
const fsp = require("fs").promises;
const path = require("path");
const { spawn } = require('node:child_process')

const DEBUG_DIR = '/Users/drogus/code/jawsm/debug/';
// Ensure debug directory exists
if (!fs.existsSync(DEBUG_DIR)) {
  fs.mkdirSync(DEBUG_DIR, { recursive: true });
}

let instance,
  pollables = [],
  pollableIndex = 0,
  scriptResult,
  pollablesToWaitForLength = 0;


const jsFile = process.argv[2] == "--test262" ? process.argv[3] : process.argv[2];

// let dir = path.dirname(jsFile);
// console.error("DIR", dir);
//
// const command = spawn('find', [dir])
// command.stdout.on('data', output => {
//     console.error("Output: ", output.toString())
// })

if (!jsFile) {
  console.error("Please provide a JavaScript file path");
  process.exit(1);
}

const jawsmPath =
  process.env.JAWSM_PATH || path.join(__dirname, "../target/release/jawsm");

function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve();
    }, Number(ms));
  });
}

class Pollable {
  constructor(promise, index) {
    this.promise = promise.then(() => index);
    this.index = index;

    promise
      .then(() => {
        this.ready = true;
      })
      .catch(() => {
        this.ready = true;
        this.failed = true;
      });
  }

  ready() {
    return this.ready;
  }

  getIndex() {
    return this.index;
  }

  getPromise() {
    return this.promise.then(() => this);
  }
}

function findPollable(id) {
  for (const pollable of pollables) {
    if (pollable.getIndex() === id) {
      return pollable;
    }
  }
}

const importObject = {
  "wasi:io/poll@0.2.1": {
    poll: function (ptr, length, returnPtr) {
      // this is a simplification, ie. arrays do not necessarily take 4 bytes of space
      // per element, but in here I know we deal with i32 values
      const pollableIds = new Uint32Array(
        instance.exports.memory.buffer,
        ptr,
        length,
      );

      const pollablesToWaitFor = [];
      for (const id of pollableIds) {
        pollablesToWaitFor.push(findPollable(id).getPromise());
      }

      pollablesToWaitForLength = pollablesToWaitFor.length;
      if (pollablesToWaitForLength === 0) {
        if (typeof process !== "undefined") {
          process.exit(scriptResult);
        }
      }

      Promise.race(pollablesToWaitFor).then((ready) => {
        const dataView = new DataView(instance.exports.memory.buffer);
        dataView.setInt32(returnPtr, 1, true);
        dataView.setInt32(returnPtr + 4, ready.getIndex(), true);

        instance.exports["main_loop"]();
      });
    },
  },
  "wasi:clocks/monotonic-clock@0.2.1": {
    "subscribe-duration": function (durationNanos) {
      let index = pollableIndex;
      let pollable = new Pollable(
        sleep(durationNanos / BigInt(1000000)),
        index,
      );
      pollables.push(pollable);
      pollableIndex++;
      return index;
    },
  },
  console: { log: (value) => console.log(`WebAssembly log: ${value}`) },
  wasi_snapshot_preview1: {
    proc_exit(code) {
      if (typeof process !== undefined) {
        process.exit(code);
      } else {
        console.log("exit code: ", code);
      }
    },
    fd_write(fd, iovsPtr, iovsLength, bytesWrittenPtr) {
      const iovs = new Uint32Array(
        instance.exports.memory.buffer,
        iovsPtr,
        iovsLength * 2,
      );
      if (fd === 1) {
        //stdout
        let text = "";
        let totalBytesWritten = 0;
        const decoder = new TextDecoder();
        for (let i = 0; i < iovsLength * 2; i += 2) {
          const offset = iovs[i];
          const length = iovs[i + 1];
          const textChunk = decoder.decode(
            new Int8Array(instance.exports.memory.buffer, offset, length),
          );
          text += textChunk;
          totalBytesWritten += length;
        }
        const dataView = new DataView(instance.exports.memory.buffer);
        dataView.setInt32(bytesWrittenPtr, totalBytesWritten, true);
        if (typeof process !== "undefined") {
          process.stdout.write(text);
        } else {
          console.log(text);
        }
      }
      return 0;
    },
  },
};

let STDERR = [];

async function runJawsm(jsFile) {
  return new Promise((resolve, reject) => {
    // Spawn the process directly instead of using execFile
    const proc = spawn(jawsmPath, [jsFile], {
      //stdio: ['inherit', 'pipe', 'inherit']
    });
    
    const chunks = [];
    
    proc.stdout.on('data', (chunk) => {
      chunks.push(chunk);
    });

    proc.stderr.on('data', (chunk) => {
      STDERR.push(chunk);
    });

    proc.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Process exited with code ${code}`));
        return;
      }
      // Concatenate all chunks into a single buffer
      const buffer = Buffer.concat(chunks);
      resolve(buffer);
    });
    
    proc.on('error', (err) => {
      reject(err);
    });
  });
}

(async function () {
  try {
    const bytes = await runJawsm(jsFile);
    let compiled = await WebAssembly.compile(bytes, { builtins: ["js-string"] });
    instance = await WebAssembly.instantiate(compiled, importObject);
    const exports = instance.exports;

    scriptResult = exports["wasi:cli/run@0.2.1#run"]();
    if (pollablesToWaitForLength === 0) {
      if (typeof process !== "undefined") {
        process.exit(scriptResult);
      }
    }
  } catch(error) {
    // For some reason the test262 runner will not properly pass a test that asserts
    // for a SyntaxError if there is also any other error present. On the other hand
    // it will also *pass* a test that should fail if there is no error message present,
    // even though the exit code was 0. That's why we check if STDERR from the JAWSM
    // run has any output - if it does, it means that it was JAWSM that failed. Otherwise
    // we output the error we got from running WASM
    let stderr = STDERR.join("");
    if (stderr == "") {
      console.error(error);
    } else {
      console.error(stderr);
    }
    process.exit(1);
  }
})();
