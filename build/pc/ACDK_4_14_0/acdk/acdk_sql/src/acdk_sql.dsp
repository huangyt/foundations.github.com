# Microsoft Developer Studio Project File - Name="acdk_sql" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_sql - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_sql.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_sql.mak" CFG="acdk_sql - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_sql - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_sql - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_sql - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_sql\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_sql\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_SQL_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_sql_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_sql - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_sql\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_sql\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_SQL_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_sql_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_sql - Win32 Release"
# Name "acdk_sql - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "sql"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/sql\Array.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Blob.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\CallableStatement.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Connection.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\DatabaseMetaData.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Driver.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\DriverManager.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\DriverPropertyInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\PreparedStatement.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\ResultSet.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\ResultSetMetaData.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\sql.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\SQLConfig.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\SQLException.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\SQLWarning.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\sql_all.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Statement.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Time.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Timestamp.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Types.h
# End Source File

# Begin Source File
SOURCE=acdk/sql\Connection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\DatabaseMetaData.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\DriverManager.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\ResultSet.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\sql_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\Time.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\Timestamp.cpp
# End Source File

# Begin Source File
SOURCE=acdk/sql\Types.cpp
# End Source File
# end Group
# end Group


# End Target
# End Project
