let instance,
  pollables = [],
  pollableIndex = 0;

function sleep(ms) {
  return new Promise((resolve) =>
    setTimeout(() => {
      resolve();
    }, Number(ms)),
  );
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
    poll: async function (ptr, length, returnPtr) {
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

      let ready = await Promise.race(pollablesToWaitFor);

      const dataView = new DataView(instance.exports.memory.buffer);
      dataView.setInt32(returnPtr, 1, true);
      dataView.setInt32(returnPtr + 4, ready.getIndex(), true);

      instance.exports["main_loop"]();
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

(async function () {
  let bytes;
  if (typeof process !== "undefined") {
    const fs = require("node:fs");
    const path = process.argv[2] || "out.wasm";
    bytes = fs.readFileSync(path, { encoding: null });
  } else if (typeof read !== "undefined") {
    bytes = read("wasm/generated.wasm", "binary");
  } else {
    const response = await fetch("out.wasm");
    bytes = await response.arrayBuffer();
  }
  let compiled = await WebAssembly.compile(bytes, { builtins: ["js-string"] });
  instance = await WebAssembly.instantiate(compiled, importObject);
  const exports = instance.exports;

  let result = exports["wasi:cli/run@0.2.1#run"]();

  if (typeof process !== undefined) {
    process.exit(result);
  }
})();
