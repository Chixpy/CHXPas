@echo off
setlocal enabledelayedexpansion
chcp 65001 > nul

pushd "%~dp0"
mkdir bin > nul
mkdir ..\..\tmp\lib > nul

echo Compilando Primitives...
fpc @fpcfg.cfg %*
set "ERRCOMP=!ERRORLEVEL!"

popd

exit /b %ERRCOMP%
