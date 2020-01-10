#!/bin/sh
./test-build.sh || exit 1
tests/test.sh || exit 1
