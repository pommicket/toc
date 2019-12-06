#!/bin/bash
DIR=$(dirname $0)
TOC=$DIR/../toc
CFLAGS="-g -Wno-parentheses-equality"
if [ "$COMPILERS" = "" ]; then
	COMPILERS="gcc tcc clang"
fi

echo $$

compile() {
	$CC $EXTRA_CFLAGS $CFLAGS -o $DIR/$1/$1.bin $DIR/$1/$1.c || exit 1
}

do_tests() {
	valgrind -q $TOC "$DIR/$1/$1.toc" -o "$DIR/$1/$1.c" > /dev/null || exit 1
	for CC in $COMPILERS; do

		for EXTRA_CFLAGS in "-O0 -g" "-O3 -s"; do
			printf "Running test $1 with C compiler $CC and flags $EXTRA_CFLAGS... "
			compile "$1"
			cd "$DIR/$1"
			./test.sh || { printf "\x1b[91mfailed!\x1b[0m\n"; exit 1; }
			printf '\x1b[92mpassed!\x1b[0m\n'
			cd $STARTPWD
		done
	done
	
}

STARTPWD="$(pwd)"

do_tests bf
do_tests arr
