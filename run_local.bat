@echo off
setlocal

set PORT=3838
if not "%~1"=="" set PORT=%~1

echo Starting designplot LOCAL server on 127.0.0.1:%PORT% ...
Rscript run_local.R port=%PORT%

endlocal
