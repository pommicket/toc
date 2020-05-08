@echo off
pushd "%~dp0"
cl /nologo tests.c
tests.exe
popd
