#!/bin/sh
echo "tcc -w main.c -o toc" && tcc -w main.c -o toc || exit 1
for CC in tcc gcc clang g++ ; do
	CC="$CC" CFLAGS='-Werror' ./build.sh || exit 1
	CC="$CC" CFLAGS='-Werror' ./build.sh release || exit 1
done
CC=''
CFLAGS='-Werror' ./build.sh release || exit 1
CFLAGS='-Werror' ./build.sh || exit 1
