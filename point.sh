#!/bin/sh
valgrind -q ./toc point.toc -o point.c || exit 1
valgrind -q ./toc test.toc || exit 1
gcc point.c out.c -o a.out -O0 -g || exit 1
./a.out || exit 1
