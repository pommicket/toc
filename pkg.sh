#!/bin/sh
valgrind -q ./toc $1.toc -o $1.c || exit 1
valgrind -q ./toc test.toc || exit 1
gcc test.toc $1.c || exit 1
./a.out
