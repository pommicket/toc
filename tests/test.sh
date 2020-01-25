#!/bin/bash
DIR=$(dirname $0)
TOC=$DIR/../toc
CFLAGS="-g -Wno-parentheses-equality"
echo $$

compile() {
	EXTRA_FLAGS=""
	if [ "$CC" = "gcc -O0 -g" ]; then
		EXTRA_FLAGS="-Wno-builtin-declaration-mismatch"
	elif [ "$CC" = "clang -O3 -s" ]; then
		EXTRA_FLAGS="-Wno-builtin-requires-header"
	fi
	$CC $CFLAGS $EXTRA_FLAGS -o $DIR/$1/$1.bin $DIR/$1/$1.c || exit 1
}

do_tests() {
	valgrind  -q --exit-on-first-error=yes --error-exitcode=1 $TOC "$DIR/$1/$1.toc" -o "$DIR/$1/$1.c" >/dev/null || exit 1
	for CC in "gcc -O0 -g" "tcc" "clang -O3 -s"; do
		printf "Running test $1 with C compiler $CC... "
		compile "$1"
		cd "$DIR/$1"
		./test.sh || { printf "\x1b[91mfailed!\x1b[0m\n"; exit 1; }
		printf '\x1b[92mpassed!\x1b[0m\n'
		cd $STARTPWD
	done
	
}

STARTPWD="$(pwd)"

do_tests bf
do_tests arr
do_tests arr2
do_tests foreign
do_tests params
