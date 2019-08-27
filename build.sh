#!/bin/bash
CC=gcc
$CC -o toc main.c -O0 -g -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wno-unused-function -Wno-unused-parameter -std=c11 || exit 1
