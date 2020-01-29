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
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wimplicit-fallthrough -Wno-missing-braces'
elif [ "$CC" = "gcc" ]; then
	WARNINGS='-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wno-pointer-to-int-cast'
elif [ "$CC" = "tcc" ]; then
	WARNINGS='-w'
elif [ "$CC" = "g++" ]; then
	WARNINGS='-w -fpermissive'
else
	WARNINGS=''
fi

if [ "$COMPILE_TIME_FOREIGN_FN_SUPPORT" != "no" ]; then
	ADDITIONAL_FLAGS="$ADDITIONAL_FLAGS -DCOMPILE_TIME_FOREIGN_FN_SUPPORT=1 -lffcall -ldl"
fi


DEBUG_FLAGS="-O0 $WARNINGS -std=c11 -DTOC_DEBUG"
if [ "$CC" = "gcc" ]; then
	DEBUG_FLAGS="$DEBUG_FLAGS -no-pie -gdwarf-2 -pipe"
fi
RELEASE_FLAGS="-O3 -s -DNDEBUG $WARNINGS -std=c11"

if [ "$1" = "release" ]; then
	FLAGS="$RELEASE_FLAGS $ADDITIONAL_FLAGS"
else
	FLAGS="$DEBUG_FLAGS $ADDITIONAL_FLAGS"
fi

COMMAND="$CC $FLAGS -o toc main.c"
echo $COMMAND
$COMMAND || exit 1
