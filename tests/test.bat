@echo off
pushd "%~dp0"
cl /nologo test.c
test
popd
