#!/bin/sh
echo "tcc -DCOMPILE_TIME_FOREIGN_FN_SUPPORT=0 -Wall main.c -o toc" && tcc -DCOMPILE_TIME_FOREIGN_FN_SUPPORT=0 -Wall main.c -o toc || exit 1 # test without compile time foreign fn support
for CC in tcc gcc clang g++ ; do
	CC="$CC" CFLAGS='-Werror' ./build.sh || exit 1
	CC="$CC" CFLAGS='-Werror' ./build.sh release || exit 1
done
CC=''
CFLAGS='-Werror' ./build.sh release || exit 1
CFLAGS='-Werror' ./build.sh || exit 1
