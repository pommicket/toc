#!/bin/sh
std_things="io arr"
cd std

VALGRIND="valgrind --track-origins=yes --exit-on-first-error=yes --error-exitcode=1 -q"
if [ "$1" = "nov" ]; then
	VALGRIND=""
fi
if [ "$CC" = "" ]; then
	CC=gcc
fi

for thing in $std_things; do
	echo "compiling $thing"
	$VALGRIND ../toc $thing.toc -o $thing.c || exit 1
done
cd ..
echo "compiling test.toc"
$VALGRIND ./toc test.toc || exit 1
$CC out.c std/*.c -Wno-builtin-declaration-mismatch || exit 1
./a.out
