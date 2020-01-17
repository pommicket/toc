#!/bin/sh
std_things="io arr"
cd std
for thing in $std_things; do
	valgrind --track-origins=yes --exit-on-first-error=yes --error-exitcode=1 -q ../toc $thing.toc -o $thing.c || exit 1
done
cd ..
valgrind --track-origins=yes --exit-on-first-error=yes --error-exitcode=1 -q ./toc test.toc || exit 1
gcc out.c std/*.c || exit 1
./a.out
