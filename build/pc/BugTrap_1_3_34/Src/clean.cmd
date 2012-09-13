del /s/q/f *.ncb
del /s/q/f *.aps
del /s/q/f *.log
del /s/q/f/a:h *.suo

rem === Win32 ==================================================

rd /s/q "Win32\Bin"

rd /s/q "Win32\BugTrap\Debug"
rd /s/q "Win32\BugTrap\Release"
rd /s/q "Win32\BugTrap\UDebug"
rd /s/q "Win32\BugTrap\URelease"
rd /s/q "Win32\BugTrap\NDebug"
rd /s/q "Win32\BugTrap\NRelease"
rd /s/q "Win32\BugTrap\Debug-x64"
rd /s/q "Win32\BugTrap\Release-x64"
rd /s/q "Win32\BugTrap\UDebug-x64"
rd /s/q "Win32\BugTrap\URelease-x64"
rd /s/q "Win32\BugTrap\NDebug-x64"
rd /s/q "Win32\BugTrap\NRelease-x64"
rd /s/q "Win32\BugTrap\HTML"

rd /s/q "Win32\Examples\BugTrapTest\Debug"
rd /s/q "Win32\Examples\BugTrapTest\Release"
rd /s/q "Win32\Examples\BugTrapTest\UDebug"
rd /s/q "Win32\Examples\BugTrapTest\URelease"
rd /s/q "Win32\Examples\BugTrapTest\Debug-x64"
rd /s/q "Win32\Examples\BugTrapTest\Release-x64"
rd /s/q "Win32\Examples\BugTrapTest\UDebug-x64"
rd /s/q "Win32\Examples\BugTrapTest\URelease-x64"

rd /s/q "Win32\Examples\BugTrapConsoleTest\Debug"
rd /s/q "Win32\Examples\BugTrapConsoleTest\Release"
rd /s/q "Win32\Examples\BugTrapConsoleTest\UDebug"
rd /s/q "Win32\Examples\BugTrapConsoleTest\URelease"
rd /s/q "Win32\Examples\BugTrapConsoleTest\Debug-x64"
rd /s/q "Win32\Examples\BugTrapConsoleTest\Release-x64"
rd /s/q "Win32\Examples\BugTrapConsoleTest\UDebug-x64"
rd /s/q "Win32\Examples\BugTrapConsoleTest\URelease-x64"

rd /s/q "Win32\Examples\BugTrapLogTest\Debug"
rd /s/q "Win32\Examples\BugTrapLogTest\Release"
rd /s/q "Win32\Examples\BugTrapLogTest\UDebug"
rd /s/q "Win32\Examples\BugTrapLogTest\URelease"
rd /s/q "Win32\Examples\BugTrapLogTest\Debug-x64"
rd /s/q "Win32\Examples\BugTrapLogTest\Release-x64"
rd /s/q "Win32\Examples\BugTrapLogTest\UDebug-x64"
rd /s/q "Win32\Examples\BugTrapLogTest\URelease-x64"

rd /s/q "Win32\Examples\BugTrapManCppTest\Debug"
rd /s/q "Win32\Examples\BugTrapManCppTest\Release"

rd /s/q "Win32\Examples\BugTrapNetTest\bin"
rd /s/q "Win32\Examples\BugTrapNetTest\obj"

rd /s/q "Win32\CrashExplorer\Debug"
rd /s/q "Win32\CrashExplorer\Release"
rd /s/q "Win32\CrashExplorer\HTML"

rem === Server =================================================

rd /s/q "Server\BugTrapServer\bin"
rd /s/q "Server\BugTrapServer\obj"
rd /s/q "Server\BugTrapServer\HTML"

rd /s/q "Server\BugTrapWebServer\PrecompiledWeb"

rd /s/q "Server\JBugTrapServer\classes"
rd /s/q "Server\JBugTrapServer\HTML"

del /f/q "Server\JBugTrapServer\BugTrapServer.map"
del /f/q "Server\JBugTrapServer\JBugTrapServer.jar"

rem === Common =================================================

rd /s/q "Setup\Output"
