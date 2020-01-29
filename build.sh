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

if [ "$1" = "" ]; then
	if [ "$COMPILE_TIME_FOREIGN_FN_SUPPORT" != "no" ]; then
		ADDITIONAL_FLAGS="$ADDITIONAL_FLAGS -DCOMPILE_TIME_FOREIGN_FN_SUPPORT=1 -lffcall -ldl"
	fi
fi

DEBUG_FLAGS="-O0 -no-pie -gdwarf-2 -pipe $WARNINGS -std=c11 -DTOC_DEBUG"
RELEASE_FLAGS="-O3 -s -DNDEBUG $WARNINGS -std=c11"

if [ "$1" = "release" ]; then
	FLAGS="$RELEASE_FLAGS $ADDITIONAL_FLAGS"

	COMMAND="$CC compatibility.c -Wall -Wextra -o compatibility"
	echo $COMMAND
	$COMMAND || exit 1
	FLAGS="$FLAGS $(./compatibility)"
else
	FLAGS="$DEBUG_FLAGS $ADDITIONAL_FLAGS"
fi

COMMAND="$CC $FLAGS -o toc main.c"
echo $COMMAND
$COMMAND || exit 1
