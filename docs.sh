#!/bin/bash
markdown README.md > README.html
for x in docs/*.md; do
	echo $x
	markdown $x > $(dirname $x)/$(basename $x .md).html
done
