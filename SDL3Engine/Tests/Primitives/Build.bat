#!/bin/bash
@echo off
setlocal enabledelayedexpansion
chcp 65001 > nul

pushd "%~dp0"
mkdir bin > nul
mkdir lib > nul

echo Compilando Primitives...
fpc @fp.cfg %*
set "ERRCOMP=!ERRORLEVEL!"

popd

exit /b %ERRCOMP%
