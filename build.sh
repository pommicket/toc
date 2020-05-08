#!/bin/sh
[ "$NASM" = "" ] && NASM=nasm

if [ "$CC" = "" ]; then
	if [ "$1" = "release" ]; then
		CC=clang
	else
		CC=gcc
	fi
fi

if uname | grep -qi bsd; then
	CFLAGS="$CFLAGS -L/usr/local/lib -I/usr/local/include -std=gnu99 -w"
else
	CFLAGS="$CFLAGS -Wpedantic -std=c11"
fi

ADDITIONAL_FLAGS="$CFLAGS -Wno-unused-function"

if [ "$CC" = "clang" ]; then
	WARNINGS='-Wall -Wextra -Wshadow -Wconversion -Wimplicit-fallthrough -Wno-missing-braces'
elif [ "$CC" = "gcc" ]; then
	WARNINGS='-Wall -Wextra -Wshadow -Wconversion'
elif [ "$CC" = "tcc" ]; then
	WARNINGS='-Wall'
elif [ "$CC" = "g++" ]; then
	WARNINGS='-w -fpermissive'
else
	WARNINGS=''
fi

[ "$ARCH" = "" ] && ARCH="$(uname -m)"
[ "$ARCH" = "amd64" ] && ARCH=x86_64

if [ "$COMPILE_TIME_FOREIGN_FN_SUPPORT" != "no" ]; then
	uname | grep -qi bsd || LIBRARIES="$LIBRARIES -ldl"
	[ "$ARCH" = "x86_64" ] || LIBRARIES="$LIBRARIES -lavcall"
	ADDITIONAL_FLAGS="$ADDITIONAL_FLAGS $LIBRARIES"
else
	ADDITIONAL_FLAGS="$ADDITIONAL_FLAGS -DCOMPILE_TIME_FOREIGN_FN_SUPPORT=0"
fi


DEBUG_FLAGS="-O0 $WARNINGS -DTOC_DEBUG"
if [ "$CC" = "gcc" ]; then
	DEBUG_FLAGS="$DEBUG_FLAGS -no-pie -gdwarf-2 -pipe"
elif [ "$CC" = "clang" ]; then
	DEBUG_FLAGS="$DEBUG_FLAGS -no-pie -gdwarf-2 -pipe"
fi
RELEASE_FLAGS="-O3 -s -DNDEBUG $WARNINGS -std=c11"

if [ "$1" = "release" ]; then
	FLAGS="$RELEASE_FLAGS $ADDITIONAL_FLAGS"
else
	FLAGS="$DEBUG_FLAGS $ADDITIONAL_FLAGS"
fi

c() {
	echo "$1" && $1 || exit 1
}

[ "$ARCH" = "x86_64" ] && [ ! -f systemv64call.o ] && c "$NASM -f elf64 systemv64call.asm"
SOURCES=main.c
[ "$ARCH" = "x86_64" ] && SOURCES="$SOURCES systemv64call.o" 
c "$CC $FLAGS -o toc $SOURCES"

