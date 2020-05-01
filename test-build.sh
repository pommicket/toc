#!/bin/sh
echo "tcc -Wall main.c -o toc" && tcc -Wall main.c -o toc || exit 1 # test without any compiler flags
for CC in tcc gcc clang g++ ; do
	CC="$CC" CFLAGS='-Werror' ./build.sh || exit 1
	CC="$CC" CFLAGS='-Werror' ./build.sh release || exit 1
done
CC=''
CFLAGS='-Werror' ./build.sh release || exit 1
CFLAGS='-Werror' ./build.sh || exit 1
