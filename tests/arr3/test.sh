#!/bin/sh
./arr3.bin > got || exit 1
diff got expected || exit 1
