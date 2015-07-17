@echo off

REM 64-bit
rd /s /q _vs2010-64
ctest -S vs2010-64.cmake -C Release -VV -O vs2010-Release-64.log
ctest -S vs2010-64.cmake -C Debug -VV -O vs2010-Debug-64.log

REM 32-bit
rd /s /q _vs2010-32
ctest -S vs2010-32.cmake -C Release -VV -O vs2010-Release-32.log
ctest -S vs2010-32.cmake -C Debug -VV -O vs2010-Debug-32.log

REM 7zip
set NAME=%cd%
for %%f in (%NAME%) do set NAME=%%~nxf-vs2010
ren vs2010 %NAME%
"C:\Program Files\7-Zip\7z" a %NAME%.7z %NAME%
