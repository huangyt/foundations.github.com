# Microsoft Developer Studio Project File - Name="sqlite3" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=sqlite3 - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "sqlite3.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "sqlite3.mak" CFG="sqlite3 - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "sqlite3 - Win32 Release" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE "sqlite3 - Win32 Debug" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "sqlite3 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\sqlite3\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\sqlite3\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /I "../src"  /I "../include"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /debug /debugtype:coff /machine:I386  /out:"..\..\bin\sqlite3_r.exe" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "sqlite3 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\sqlite3\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\sqlite3\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /I "../src"  /I "../include"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /debug /pdbtype:sept /machine:I386  /out:"..\..\bin\sqlite3_d.exe" ""

!ENDIF 

# Begin Target

# Name "sqlite3 - Win32 Release"
# Name "sqlite3 - Win32 Debug"


# Begin Group "sqlitesrc"
# PROP Default_Filter ""

# Begin Source File
SOURCE=sqlitesrc/alter.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/attach.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/auth.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/btree.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/build.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/date.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/delete.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/expr.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/func.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/hash.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/insert.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/legacy.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/main.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/opcodes.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/os_unix.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/os_win.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/pager.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/parse.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/pragma.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/printf.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/random.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/select.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/shell.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/table.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/tokenize.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/trigger.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/update.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/utf.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/util.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/vacuum.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/vdbe.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/vdbeapi.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/vdbeaux.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/vdbemem.c
# End Source File

# Begin Source File
SOURCE=sqlitesrc/where.c
# End Source File
# end Group


# End Target
# End Project
