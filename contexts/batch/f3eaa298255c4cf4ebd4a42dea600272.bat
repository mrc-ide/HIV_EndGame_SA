@echo off
REM automatically generated
ECHO generated on host: WPIA-DIDE299
ECHO generated on date: 2022-12-16
ECHO didehpc version: 0.3.18
ECHO context version: 0.3.0
ECHO running on: %COMPUTERNAME%
set CONTEXT_WORKDRIVE=Q:
set CONTEXT_WORKDIR=Git\HIV_EndGame_SA
set CONTEXT_ROOT=Q:\Git\HIV_EndGame_SA\contexts
set CONTEXT_ID=59dbb071edd7faeb3335cc562b3aa27a
set R_LIBS_USER=Q:\Git\HIV_EndGame_SA\contexts\lib\windows\4.1
call setr64_4_1_0.bat
REM If Java is wanted, then call setJava64.
REM If called with blank, it adds default JRE.
IF 'FALSE'=='TRUE' (
  call setJava64.bat 
)
ECHO mapping Q: -^> \\fi--san03.dide.ic.ac.uk\homes\spr21
net use Q: \\fi--san03.dide.ic.ac.uk\homes\spr21 /y
ECHO mapping T: -^> \\fi--didef3.dide.ic.ac.uk\tmp
net use T: \\fi--didef3.dide.ic.ac.uk\tmp /y
set REDIS_HOST=11.0.0.1
set REDIS_URL=redis://11.0.0.1:6379
%CONTEXT_WORKDRIVE%
cd \%CONTEXT_WORKDIR%
ECHO working directory: %CD%
ECHO this is a single task
set CONTEXT_TASK_ID=f3eaa298255c4cf4ebd4a42dea600272
set CONTEXT_LOGFILE=Q:\Git\HIV_EndGame_SA\contexts\logs\%CONTEXT_TASK_ID%
ECHO logfile: %CONTEXT_LOGFILE%
@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "Q:\Git\HIV_EndGame_SA\contexts\bin\task_run" "%CONTEXT_ROOT%" %CONTEXT_TASK_ID% > "%CONTEXT_LOGFILE%" 2>&1
@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%
ECHO Removing mapping Q:
net use Q: /delete /y
ECHO Removing mapping T:
net use T: /delete /y
set ERRORLEVEL=%ErrorCode%
if %ERRORLEVEL% neq 0 (
  ECHO Error running task
  EXIT /b %ERRORLEVEL%
)
@ECHO Quitting
