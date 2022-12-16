@echo off
ECHO generated on host: WPIA-DIDE299
ECHO generated on date: 2022-12-16
ECHO didehpc version: 0.3.18
ECHO conan version: 0.1.1
ECHO running on: %COMPUTERNAME%
call setr64_4_1_0.bat
ECHO mapping Q: -^> \\fi--san03.dide.ic.ac.uk\homes\spr21
net use Q: \\fi--san03.dide.ic.ac.uk\homes\spr21 /y
ECHO mapping T: -^> \\fi--didef3.dide.ic.ac.uk\tmp
net use T: \\fi--didef3.dide.ic.ac.uk\tmp /y
set CONAN_PATH_BOOTSTRAP=T:\conan\bootstrap\4.1
set CONAN_PATH_CACHE=Q:\contexts\conan\cache
set CONAN_ID=8d0d411b72e5c5b800f7b4ecacf6fdaf
set CONAN_LOGFILE=Q:\contexts\conan\log\%CONAN_ID%
ECHO logfile: %CONAN_LOGFILE%
Q:
cd \
ECHO working directory: %CD%
ECHO on
Rscript "Q:\contexts\conan\bin\%CONAN_ID%" "Q:\contexts\lib\windows\4.1" > "%CONAN_LOGFILE%" 2>&1
@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%
ECHO Removing mapping Q:
net use Q: /delete /y
ECHO Removing mapping T:
net use T: /delete /y
set ERRORLEVEL=%ErrorCode%
if %ERRORLEVEL% neq 0 (
  ECHO Error running conan
  EXIT /b %ERRORLEVEL%
)
@ECHO Quitting
