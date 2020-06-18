@echo off

if NOT EXIST build mkdir build

echo Building assets...
python bin\png2arc.py -pad -o build\title.bin -p build\title.pal data\gfx\title-16.png 9

python bin\png2arc.py -pad -o build\slide_01.bin -p build\slide_01.pal data\gfx\slide_01.png 9
python bin\png2arc.py -pad -o build\slide_02.bin -p build\slide_02.pal data\gfx\slide_02.png 9
python bin\png2arc.py -pad -o build\slide_03.bin -p build\slide_03.pal data\gfx\slide_03.png 9
python bin\png2arc.py -pad -o build\slide_04.bin -p build\slide_04.pal data\gfx\slide_04.png 9
python bin\png2arc.py -pad -o build\slide_05.bin -p build\slide_05.pal data\gfx\slide_05.png 9

python bin\png2arc.py -pad -o build\patarty.bin -p build\patarty.pal data\gfx\patarty16.png 9
python bin\png2arc.py -pad -o build\logo.bin -p build\logo.pal data\gfx\logo_reflection06.png 9

bin\lz4.exe -f build\slide_01.bin build\slide_01.lz4
bin\lz4.exe -f build\slide_02.bin build\slide_02.lz4
bin\lz4.exe -f build\slide_03.bin build\slide_03.lz4
bin\lz4.exe -f build\slide_04.bin build\slide_04.lz4
bin\lz4.exe -f build\slide_05.bin build\slide_05.lz4

bin\lz4.exe -f build\patarty.bin build\patarty.lz4
bin\lz4.exe -f build\logo.bin build\logo.lz4

echo Assembling code...
..\..\vasm\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\stniccc.bin stniccc.asm

if %ERRORLEVEL% neq 0 (
	echo Failed to assemble code.
	exit /b 1
)

echo Making !folder...
set FOLDER="!Demand"
if EXIST %FOLDER% del /Q "%FOLDER%"
if NOT EXIST %FOLDER% mkdir %FOLDER%

echo Adding files...
copy folder\*.* "%FOLDER%\*.*"
copy build\stniccc.bin "%FOLDER%\!RunImage,ff8"
copy data\stniccc\scene1.bin "%FOLDER%\Scene1,ffd"
copy data\music\Arc-NIC5.mod "%FOLDER%\Music,001"

echo Copying !folder...
set HOSTFS=..\..\Arculator_V2.0_Windows\hostfs
if EXIST "%HOSTFS%\%FOLDER%" del /Q "%HOSTFS%\%FOLDER%"
if NOT EXIST "%HOSTFS%\%FOLDER%" mkdir "%HOSTFS%\%FOLDER%"
copy "%FOLDER%\*.*" "%HOSTFS%\%FOLDER%"
