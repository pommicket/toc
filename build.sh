#!/bin/bash
gcc -o toc main.c -g -o toc -Wall -Wextra -Wpedantic -Wconversion -std=c11 || exit 1
