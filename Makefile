# Wrapper so that `make` works from PowerShell / Windows Terminal.
# GNU make runs recipes with cmd.exe here (no sh on PATH).
# make.bat assembles into build\ and then copies the result into Arculator hostfs (..\..\Arculator\hostfs\Builds).
.PHONY: all build help
all: build

build:
	cmd /c .\make.bat

help:
	@echo make        - build (runs make.bat; copies to Arculator hostfs (..\..\Arculator\hostfs\Builds))
	@echo make help   - this list
	@echo See START_HERE.md
