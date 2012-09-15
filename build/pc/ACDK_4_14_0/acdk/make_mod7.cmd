cd %1
if %2==install nmake -f %1.nmake install
if %2==all nmake -f %1.nmake install
REM if %2==compile nmake -f %1.nmake install
if %2==compile devenv %1.sln /build debug
if %2==all devenv %1.sln /build debug
if %2==all devenv %1.sln /build release
if %2==clean devenv %1.sln /clean DEBUG
if %2==clean devenv %1.sln /clean RELEASE
if %2==test nmake -f %1.nmake test
cd ..
