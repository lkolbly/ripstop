#!/bin/bash

set -e

for PYBIN in /opt/python/cp3[6789]*/bin; do
    "${PYBIN}/pip" install maturin
done

/opt/python/cp310-cp310/bin/pip install maturin
