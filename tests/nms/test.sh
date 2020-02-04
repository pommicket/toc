#!/bin/sh
./nms.bin > got || exit 1
diff got expected || exit 1
