#!/bin/bash
gcc -o toc main.c -O0 -g -o toc -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wno-unused-function -Wno-unused-parameter -std=c11 || exit 1
