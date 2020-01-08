#!/bin/sh
./build.sh || exit 1
./build.sh release || exit 1
for CC in tcc gcc clang g++ ; do
	
	CC="$CC" ./build.sh || exit 1
	CC="$CC" ./build.sh release || exit 1
done
