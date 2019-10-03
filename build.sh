#!/bin/bash
CC=gcc

# Possible extra build flags
# these are for compiling the compiler, and NOT for compiling the program itself.
# -DNONZERO_NULL_PTRS
#   - must be set if the zero value of a pointer (as might be set by calloc/memset)
#     is not the NULL pointer.

ADDITIONAL_FLAGS='-Wno-unused-function'

if [[ $CC == "clang" ]]; then
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wimplicit-fallthrough'
else
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wno-pointer-to-int-cast'
fi

DEBUG_FLAGS="-O0 -g3 $WARNINGS -std=c11 -DTOC_DEBUG"
RELEASE_FLAGS="-O3 -s -DNDEBUG $WARNINGS -std=c11"

COMMAND="$CC $DEBUG_FLAGS $ADDITIONAL_FLAGS -o toc main.c"
echo $COMMAND
$COMMAND || exit 1
