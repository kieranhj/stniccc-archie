@echo off

if NOT EXIST build mkdir build

echo Building assets...
python bin\png2arc.py -o build\title.bin -p build\title.pal data\gfx\title-16.png 9

python bin\png2arc.py -o build\slide_01.bin -p build\slide_01.pal data\gfx\slide_01.png 9
python bin\png2arc.py -o build\slide_02.bin -p build\slide_02.pal data\gfx\slide_02.png 9
python bin\png2arc.py -o build\slide_03.bin -p build\slide_03.pal data\gfx\slide_03.png 9
python bin\png2arc.py -o build\slide_04.bin -p build\slide_04.pal data\gfx\slide_04.png 9
python bin\png2arc.py -o build\slide_05.bin -p build\slide_05.pal data\gfx\slide_05.png 9
python bin\png2arc.py -o build\slide_06.bin -p build\slide_06.pal data\gfx\slide_06.png 9
python bin\png2arc.py -o build\slide_07.bin -p build\slide_07.pal data\gfx\slide_07.png 9
python bin\png2arc.py -o build\slide_08.bin -p build\slide_08.pal data\gfx\slide_08.png 9
python bin\png2arc.py -o build\slide_09.bin -p build\slide_09.pal data\gfx\slide_09.png 9
python bin\png2arc.py -o build\slide_10.bin -p build\slide_10.pal data\gfx\slide_10.png 9
python bin\png2arc.py -o build\slide_11.bin -p build\slide_11.pal data\gfx\slide_11.png 9
python bin\png2arc.py -o build\slide_12.bin -p build\slide_12.pal data\gfx\slide_12.png 9
python bin\png2arc.py -o build\slide_13.bin -p build\slide_13.pal data\gfx\slide_13.png 9
python bin\png2arc.py -o build\slide_14.bin -p build\slide_14.pal data\gfx\slide_14.png 9
python bin\png2arc.py -o build\slide_15.bin -p build\slide_15.pal data\gfx\slide_15.png 9
python bin\png2arc.py -o build\slide_16.bin -p build\slide_16.pal data\gfx\slide_16.png 9
python bin\png2arc.py -o build\slide_17.bin -p build\slide_17.pal data\gfx\slide_17.png 9
python bin\png2arc.py -o build\slide_18.bin -p build\slide_18.pal data\gfx\slide_18.png 9
python bin\png2arc.py -o build\slide_19.bin -p build\slide_19.pal data\gfx\slide_19.png 9
python bin\png2arc.py -o build\slide_20.bin -p build\slide_20.pal data\gfx\slide_20.png 9

python bin\png2arc.py -o build\logo.bin -p build\logo.pal data\gfx\logo_reflection06_fixed.png 9
python bin\png2arc.py -o build\patarty.bin -p build\patarty.pal data\gfx\patarty16.png 9

bin\lz4.exe -f build\slide_01.bin build\slide_01.lz4
bin\lz4.exe -f build\slide_02.bin build\slide_02.lz4
bin\lz4.exe -f build\slide_03.bin build\slide_03.lz4
bin\lz4.exe -f build\slide_04.bin build\slide_04.lz4
bin\lz4.exe -f build\slide_05.bin build\slide_05.lz4
bin\lz4.exe -f build\slide_06.bin build\slide_06.lz4
bin\lz4.exe -f build\slide_07.bin build\slide_07.lz4
bin\lz4.exe -f build\slide_08.bin build\slide_08.lz4
bin\lz4.exe -f build\slide_09.bin build\slide_09.lz4
bin\lz4.exe -f build\slide_10.bin build\slide_10.lz4
bin\lz4.exe -f build\slide_11.bin build\slide_11.lz4
bin\lz4.exe -f build\slide_12.bin build\slide_12.lz4
bin\lz4.exe -f build\slide_13.bin build\slide_13.lz4
bin\lz4.exe -f build\slide_14.bin build\slide_14.lz4
bin\lz4.exe -f build\slide_15.bin build\slide_15.lz4
bin\lz4.exe -f build\slide_16.bin build\slide_16.lz4
bin\lz4.exe -f build\slide_17.bin build\slide_17.lz4
bin\lz4.exe -f build\slide_18.bin build\slide_18.lz4
bin\lz4.exe -f build\slide_19.bin build\slide_19.lz4
bin\lz4.exe -f build\slide_20.bin build\slide_20.lz4

bin\lz4.exe -f build\logo.bin build\logo.lz4
bin\lz4.exe -f build\patarty.bin build\patarty.lz4

echo Assembling code...
bin\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\stniccc.bin stniccc.asm

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
