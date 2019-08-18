#!/bin/bash

gcc -o toc main.c -g -o toc -Wall -Wextra -Wpedantic -Wconversion -Wno-unused-function -std=c11 || exit 1
