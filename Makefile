# Wrapper so that `make` works from PowerShell / Windows Terminal.
# GNU make runs recipes with cmd.exe here (no sh on PATH).
# make.bat assembles into build\ and then copies the result into Arculator 2.0 hostfs (..\..\Arculator_V2.0_Windows\hostfs).
.PHONY: all build help
all: build

build:
	cmd /c .\make.bat

help:
	@echo make        - build (runs make.bat; copies to Arculator 2.0 hostfs (..\..\Arculator_V2.0_Windows\hostfs))
	@echo make help   - this list
	@echo See START_HERE.md
