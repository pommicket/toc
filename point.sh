#!/bin/sh
valgrind -q ./toc point.toc -o point.c
valgrind -q ./toc test.toc
gcc point.c out.c -o a.out
./a.out
