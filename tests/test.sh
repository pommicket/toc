#!/bin/bash

tests='bf
varargs
arr
arr2
arr3
foreign
params
nms
misc'

STARTPWD=$(pwd)
cd $(pwd)/$(dirname $0)
TOC=../toc
CFLAGS="-g -Wno-parentheses-equality"
echo $$

compile_c() {
	EXTRA_FLAGS=""
	if [ "$CC" = "gcc -O0 -g" ]; then
		EXTRA_FLAGS="-Wno-builtin-declaration-mismatch"
	elif [ "$CC" = "clang -O3 -s" ]; then
		EXTRA_FLAGS="-Wno-builtin-requires-header"
	elif [ "$CC" = "tcc" ]; then
		EXTRA_FLAGS="-w"
	fi
	$CC $CFLAGS $EXTRA_FLAGS -o a.out out.c || exit 1
}

do_tests() {
	valgrind  -q --exit-on-first-error=yes --error-exitcode=1 $TOC "$1.toc" -o out.c || exit 1
	for CC in "gcc -O0 -g" "tcc" "clang -O3 -s"; do
		printf "Running test $1 with C compiler $CC... "
		compile_c "$1"
		./a.out > got
		if diff "$1"_expected got; then
			printf '\x1b[92mpassed!\x1b[0m\n'
		else
			printf '\x1b[91mfailed!\x1b[0m\n'
			exit 1
		fi
	done
}
for x in $tests; do
	do_tests "$x"
done

rm got a.out out.c
