@echo off
REM driver for exectable selecting release and debug version

set EXECUTABLE=%ACDKHOME%\bin\acdkmc

IF exist %EXECUTABLE%.exe (
  %EXECUTABLE% %1 %2 %3 %4 %5 %6 %7 %8 %9
  goto ende
)
IF exist %EXECUTABLE%_r.exe (
  %EXECUTABLE%_r %1 %2 %3 %4 %5 %6 %7 %8 %9
  goto ende
} 
IF exist %EXECUTABLE%_d.exe (
  %EXECUTABLE%_d %1 %2 %3 %4 %5 %6 %7 %8 %9
  goto ende
) 

echo Executable %EXECUTABLE% cannot be found in any variant (_d, _r)
:ende
