@echo off
..\..\vasm\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\stniccc.bin stniccc.asm
if %ERRORLEVEL%==0 copy build\stniccc.bin "..\..\Arculator_V2.0_Windows\hostfs\stniccc,ff8"
