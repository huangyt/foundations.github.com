# Microsoft Developer Studio Project File - Name="acdk_sql_sqlite" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_sql_sqlite - Win32 Debug
!MESSAGE Dies ist kein g�ltiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und f�hren Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_sql_sqlite.mak".
!MESSAGE 
!MESSAGE Sie k�nnen beim Ausf�hren von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_sql_sqlite.mak" CFG="acdk_sql_sqlite - Win32 Debug"
!MESSAGE 
!MESSAGE F�r die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_sql_sqlite - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_sql_sqlite - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_sql_sqlite - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_sql_sqlite\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_sql_sqlite\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_SQL_SQLITE_LIB" -D "THREADSAFE=1" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib ..\..\bin\acdk_sql_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_sql_sqlite_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_sql_sqlite - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_sql_sqlite\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_sql_sqlite\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_SQL_SQLITE_LIB" -D "THREADSAFE=1" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib ..\..\bin\acdk_sql_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_sql_sqlite_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_sql_sqlite - Win32 Release"
# Name "acdk_sql_sqlite - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "sql"
# PROP Default_Filter ""
# Begin Group "sqlite"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/sql/sqlite\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteConnection.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteDatabaseMetaData.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteDriver.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LitePreparedStatement.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteResultSet.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteResultSetMetaData.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteStatement.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteTable.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\sqlite.h
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteDatabaseMetaData.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteResultSet.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteStatement.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\LiteTable.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql/sqlite\sqlite_metainf_base.cpp
# End Source File
# end Group
# end Group
# end Group
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