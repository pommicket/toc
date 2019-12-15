#!/bin/sh
./arr2.bin > got || exit 1
diff got expected > /dev/null || exit 1
