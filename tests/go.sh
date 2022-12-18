#!/bin/bash

set -e

cd "$(dirname "$0")/.."

pip install ./target/wheels/ripstop_lib-0.1.0-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl

export RIPSTOP_PATH=$(pwd)/target/x86_64-unknown-linux-musl/debug/cli

cd tests/
pytest .
