@echo off

if NOT EXIST build mkdir build
echo Assembling code...
..\..\vasm\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\stniccc.bin stniccc.asm

if %ERRORLEVEL% neq 0 (
	echo Failed to assemble code.
	exit /b 1
)

echo Building assets...
python bin\png2arc.py -pad -o build\title.bin -p build\title.pal data\title-16.png 9
python bin\png2arc.py -pad -o build\outro.bin -p build\outro.pal data\outro-16.png 9

echo Making !folder...
set FOLDER="!BSAATT"
if EXIST %FOLDER% del /Q "%FOLDER%"
if NOT EXIST %FOLDER% mkdir %FOLDER%

echo Adding files...
copy folder\*.* "%FOLDER%\*.*"
copy build\stniccc.bin "%FOLDER%\!RunImage,ff8"
copy data\scene1.bin "%FOLDER%\Scene1,ffd"
copy build\title.bin "%FOLDER%\Title,ffd"
copy build\outro.bin "%FOLDER%\Outro,ffd"
copy data\checknobankh.mod "%FOLDER%\Music,001"

echo Copying !folder...
set HOSTFS=..\..\Arculator_V2.0_Windows\hostfs
if EXIST "%HOSTFS%\%FOLDER%" del /Q "%HOSTFS%\%FOLDER%"
if NOT EXIST "%HOSTFS%\%FOLDER%" mkdir "%HOSTFS%\%FOLDER%"
copy "%FOLDER%\*.*" "%HOSTFS%\%FOLDER%"
