#!/bin/bash
CC=gcc
$CC -o toc main.c -O0 -g3 -Wall -Wextra -Wpedantic -Wconversion -Wshadow -std=c11 || exit 1
