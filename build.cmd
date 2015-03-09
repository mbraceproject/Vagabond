@echo off

.vaket\vaket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.vaket\vaket.exe restore -v
if errorlevel 1 (
  exit /b %errorlevel%
)

vackages\FAKE\tools\FAKE.exe build.fsx %*
