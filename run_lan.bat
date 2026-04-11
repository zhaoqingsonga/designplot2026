@echo off
setlocal

set HOST=0.0.0.0
set PORT=3838

if not "%~1"=="" set HOST=%~1
if not "%~2"=="" set PORT=%~2

echo Starting designplot LAN server on %HOST%:%PORT% ...
Rscript run_lan.R host=%HOST% port=%PORT%

endlocal
