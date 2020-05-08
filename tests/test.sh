#!/bin/sh
cd $(dirname $0)
cc tests.c -o tests -Wall -Wconversion -Wshadow -Werror && ./tests || exit 1
cd - >/dev/null
