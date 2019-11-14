#!/bin/sh
if [ "$CC" = "" ]; then
	if [ "$1" = "release" ]; then
		CC=clang
	else
		CC=gcc
	fi
fi

ADDITIONAL_FLAGS="$CFLAGS -Wno-unused-function"

if [ "$CC" = "clang" ]; then
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wimplicit-fallthrough'
else
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wno-pointer-to-int-cast'
fi

DEBUG_FLAGS="-O0 -g3 $WARNINGS -std=c11 -DTOC_DEBUG"
RELEASE_FLAGS="-O3 -s -DNDEBUG $WARNINGS -std=c11"

if [ "$1" = "release" ]; then
	FLAGS="$RELEASE_FLAGS $ADDITIONAL_FLAGS"
else
	FLAGS="$DEBUG_FLAGS $ADDITIONAL_FLAGS"
fi

COMMAND="$CC $FLAGS -o toc main.c"
echo $COMMAND
$COMMAND || exit 1
