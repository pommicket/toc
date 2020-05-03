#!/bin/sh
cd $(dirname $0)
cc test.c -o test -Wall -Wconversion -Wshadow -Werror && ./test
cd - >/dev/null
