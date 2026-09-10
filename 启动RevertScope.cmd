@echo off
cd /d "%~dp0"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0scripts\launch.ps1" %*
set "APP_EXIT=%ERRORLEVEL%"
if "%~1"=="" pause
exit /b %APP_EXIT%
