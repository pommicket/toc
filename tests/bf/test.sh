#!/bin/sh
if [ "$(cat hw0.bf | ./bf.bin)" != "Hello World!" ]; then
	echo "hello world 0 failed."
	exit 1
fi
if [ "$(cat hw1.bf | ./bf.bin)" != "Hello World!" ]; then
	echo "hello world 1 failed."
	exit 1
fi
