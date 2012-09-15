cd %1
REM if $2==all nmake -f %1.nmake install
REM if $2==compile nmake -f %1.nmake install
nmake -f %1.nmake %2
cd ..
