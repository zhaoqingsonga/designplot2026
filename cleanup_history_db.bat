@echo off
setlocal

set DB=data\designplot.sqlite
set KEEP_RUNS=30

if not "%~1"=="" set KEEP_RUNS=%~1

echo [1/2] DRY-RUN preview...
Rscript cleanup_history_db.R db=%DB% keep_runs=%KEEP_RUNS% apply=false
if errorlevel 1 (
  echo DRY-RUN failed. Abort.
  goto :end
)

set /p CONFIRM=Execute cleanup now? (y/N): 
if /I not "%CONFIRM%"=="Y" (
  echo Cancelled.
  goto :end
)

set /p VAC=Run VACUUM after cleanup? (y/N): 
if /I "%VAC%"=="Y" (
  echo [2/2] APPLY + VACUUM...
  Rscript cleanup_history_db.R db=%DB% keep_runs=%KEEP_RUNS% apply=true vacuum=true
) else (
  echo [2/2] APPLY...
  Rscript cleanup_history_db.R db=%DB% keep_runs=%KEEP_RUNS% apply=true
)

:end
echo Done.
endlocal
