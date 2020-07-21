@echo off

if NOT EXIST build mkdir build

echo Assembling code...
bin\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\stniccc.bin stniccc.asm

if %ERRORLEVEL% neq 0 (
	echo Failed to assemble code.
	exit /b 1
)

echo Making !folder...
set FOLDER="!DemandS"
if EXIST %FOLDER% del /Q "%FOLDER%"
if NOT EXIST %FOLDER% mkdir %FOLDER%

echo Adding files...
copy folder\*.* "%FOLDER%\*.*"
copy build\stniccc.bin "%FOLDER%\!RunImage,ff8"
copy data\stniccc\scene1.bin "%FOLDER%\Scene1,ffd"
copy data\music\Arc-NIC10.mod "%FOLDER%\Music,001"
copy data\readme.txt "%FOLDER%\Readme,fff"

echo Copying !folder...
set HOSTFS=..\..\Arculator_V2.0_Windows\hostfs
if EXIST "%HOSTFS%\%FOLDER%" del /Q "%HOSTFS%\%FOLDER%"
if NOT EXIST "%HOSTFS%\%FOLDER%" mkdir "%HOSTFS%\%FOLDER%"
copy "%FOLDER%\*.*" "%HOSTFS%\%FOLDER%"
