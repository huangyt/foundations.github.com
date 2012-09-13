This folder includes executable files of all Win32 examples as well as supplementary
data files and libraries. There is everything required for testing BugTrap for Win32.

You may find here the following files:
------------------------------------------------------------------------------------
BugTrap[U].dll                     - BugTrap DLL module.
BugTrap[U].lib                     - BugTrap library file used by linker.
dbghelp.dll                        - DbgHelp library (see "BugTrap Developer's Guide"
                                     for details).
BugTrapConsoleTest[U][D].exe       - Console test application.
BugTrapConsoleTest[U][D].pdb       - File with symbolic information used by BugTrap.
BugTrapConsoleTest[U].map          - Project map file used by CrashExplorer.
BugTrapTest[U][D].exe              - GUI test application.
BugTrapTest[U][D].pdb              - File with symbolic information used by BugTrap.
BugTrapTest[U].map                 - Project map file used by CrashExplorer.
------------------------------------------------------------------------------------
File names may vary by the following signs:
  [U]   - Unicode aware version has 'U' letter. ANSI version doesn't have 'U' letter.
  [D]   - Debug version has 'D' letter. Release version doesn't have 'D' letter.

Please see "BugTrap Developer's Guide" for additional information about file types
used by BugTrap for Win32.