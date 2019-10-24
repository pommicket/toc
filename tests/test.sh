#!/bin/sh
DIR=$(dirname $0)
TOC=$DIR/../toc
CFLAGS="-g -Wno-parentheses-equality"
if [ "$COMPILERS" = "" ]; then
	COMPILERS="gcc tcc clang"
fi

echo $$

compile() {
	$TOC $DIR/$1/$1.toc -o $DIR/$1/$1.c > /dev/null || exit 1
	$CC $EXTRA_CFLAGS $CFLAGS -o $DIR/$1/$1.bin $DIR/$1/$1.c || exit 1
}

STARTPWD="$(pwd)"

for CC in $COMPILERS; do

	for EXTRA_CFLAGS in "-O0 -g" "-O3 -s"; do
		echo "Running tests with C compiler $CC and flags $EXTRA_CFLAGS."
		printf "bf... "
		compile bf
		cd $DIR/bf
		./test.sh || exit 1
		echo "passed!"
		cd $STARTPWD
	done
done
