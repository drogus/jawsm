#!/usr/bin/env bash
set -o pipefail

# Parse options
CARGO_RUN=0
while [[ $# -gt 0 ]]; do
  case $1 in
  --cargo-run)
    CARGO_RUN=1
    shift
    ;;
  *)
    break
    ;;
  esac
done

# Set JAWSM_DIR only if JAWSM_DIR is not already set
: ${JAWSM_DIR:=.}

# Determine how to run the compiler
if [ $CARGO_RUN -eq 1 ]; then
  COMPILER="cargo run"
else
  if [ -n "$JAWSM_BINARY" ]; then
    COMPILER="$JAWSM_BINARY"
  else
    COMPILER="$JAWSM_DIR/target/release/jawsm"
  fi
fi

# Create needed directories, if needed
mkdir -p $JAWSM_DIR/wat $JAWSM_DIR/wasm

# Run the compiler
if ! $COMPILER $1 wasm/generated.wasm; then
  exit 100
fi

run_wasm() {
  node run.js $JAWSM_DIR/wasm/generated.wasm
}

# Run the WASM file
if ! run_wasm; then
  exit 101
fi
