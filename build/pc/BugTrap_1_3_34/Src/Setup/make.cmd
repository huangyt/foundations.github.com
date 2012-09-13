@echo off
rem "%ProgramFiles%\Eziriz\.NET Reactor\dotNET_Reactor.exe" -project "..\Server\BugTrapServer\BugTrapServer.nrproj"
rem "%ProgramFiles%\Eziriz\.NET Reactor\dotNET_Reactor.exe" -project "..\Server\BugTrapWebServer\BugTrapWebServer.nrproj"
"%ProgramFiles%\Inno Setup 5\iscc.exe" BugTrapSetup.iss
