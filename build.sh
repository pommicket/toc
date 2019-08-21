#!/bin/bash
gcc -o toc main.c -O0 -g -o toc -Wall -Wextra -Wpedantic -Wconversion -Wshadow -std=c11 || exit 1
