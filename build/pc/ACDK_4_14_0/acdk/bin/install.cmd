
REM Install script for NT
@echo on

if not exist "%ACDKHOME%\include" mkdir "%ACDKHOME%\include"
if not exist "%ACDKHOME%\include\tests" mkdir "%ACDKHOME%\include\tests"
if not exist "%ACDKHOME%\cfg" mkdir "%ACDKHOME%\cfg"
set XCOPYOPTS=/D /S /Y
if exist src xcopy %XCOPYOPTS% src\*.h %ACDKHOME%\include
if %WEBHOME%x==x set WEBHOME=webhome_does_not_exists
if exist src ( if exist %WEBHOME% xcopy %XCOPYOPTS% src\*.cpp %ACDKHOME%\include )

if exist tests ( if exist %WEBHOME% xcopy %XCOPYOPTS% tests\*.h %ACDKHOME%\include\tests )
if exist tests ( if exist %WEBHOME% xcopy %XCOPYOPTS% tests\*.cpp %ACDKHOME%\include\tests )


if exist cfg xcopy %XCOPYOPTS% /EXCLUDE:%ACDKHOME%\bin\install_cmd.exl cfg\*.* %ACDKHOME%\cfg 
if exist bin xcopy %XCOPYOPTS% /EXCLUDE:%ACDKHOME%\bin\install_cmd.exl bin\*.* %ACDKHOME%\bin


if "%WEBHOME%x"=="x" goto nowebhome
if not exist %WEBHOME% goto nowebhome
xcopy %XCOPYOPTS% *.htxt %WEBHOME%\%PACKAGE%
xcopy %XCOPYOPTS% *.xml %WEBHOME%\%PACKAGE%
xcopy %XCOPYOPTS% *.gif %WEBHOME%\%PACKAGE%
xcopy %XCOPYOPTS% *.jpg %WEBHOME%\%PACKAGE%
del /S /F /Q %WEBHOME%\%PACKAGE%\docs\images
xcopy /Y ChangeLog %WEBHOME%\%PACKAGE%
goto scriptext

:nowebhome
goto scriptext




:scriptext
exit 0