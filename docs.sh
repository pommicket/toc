#!/bin/sh
markdown README.md > README.html
echo README.md
for x in docs/*.md; do
	markdown $x > $(dirname $x)/$(basename $x .md).html
	echo $x
done
