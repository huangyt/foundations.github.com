set MAKE=make
cd %1
if $2==all %MAKE% -f %1.bcc install
REM if $2==compile %MAKE% -i -f  %1.bcc install
%MAKE% -i -f %1.bcc %2
cd ..
