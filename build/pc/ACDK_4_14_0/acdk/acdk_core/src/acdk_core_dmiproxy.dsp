# Microsoft Developer Studio Project File - Name="acdk_core_dmiproxy" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_core_dmiproxy - Win32 Debug
!MESSAGE Dies ist kein g�ltiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und f�hren Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_core_dmiproxy.mak".
!MESSAGE 
!MESSAGE Sie k�nnen beim Ausf�hren von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_core_dmiproxy.mak" CFG="acdk_core_dmiproxy - Win32 Debug"
!MESSAGE 
!MESSAGE F�r die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_core_dmiproxy - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_core_dmiproxy - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_core_dmiproxy - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_core_dmiproxy\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_core_dmiproxy\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "../src"  /I "../include"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_core_dmiproxy_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_core_dmiproxy - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_core_dmiproxy\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_core_dmiproxy\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "../src"  /I "../include"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_core_dmiproxy_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_core_dmiproxy - Win32 Release"
# Name "acdk_core_dmiproxy - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "lang"
# PROP Default_Filter ""
# Begin Group "lang_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lang/lang_dmiproxy\lang_dmiproxy.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lang/lang_dmiproxy\lang_dmiproxy1.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lang/lang_dmiproxy\lang_dmiproxy2.cpp
# End Source File
# end Group
# Begin Group "dmi"
# PROP Default_Filter ""
# Begin Group "dmi_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lang/dmi/dmi_dmiproxy\dmi_dmiproxy.cpp
# End Source File
# end Group
# end Group
# Begin Group "ref"
# PROP Default_Filter ""
# Begin Group "ref_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lang/ref/ref_dmiproxy\ref_dmiproxy.cpp
# End Source File
# end Group
# end Group
# Begin Group "reflect"
# PROP Default_Filter ""
# Begin Group "reflect_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lang/reflect/reflect_dmiproxy\reflect_dmiproxy.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "io"
# PROP Default_Filter ""
# Begin Group "io_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/io/io_dmiproxy\io_dmiproxy.cpp
# End Source File

# Begin Source File
SOURCE=acdk/io/io_dmiproxy\io_dmiproxy1.cpp
# End Source File

# Begin Source File
SOURCE=acdk/io/io_dmiproxy\io_dmiproxy2.cpp
# End Source File
# end Group
# end Group
# Begin Group "util"
# PROP Default_Filter ""
# Begin Group "util_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/util/util_dmiproxy\util_dmiproxy.cpp
# End Source File

# Begin Source File
SOURCE=acdk/util/util_dmiproxy\util_dmiproxy1.cpp
# End Source File

# Begin Source File
SOURCE=acdk/util/util_dmiproxy\util_dmiproxy2.cpp
# End Source File
# end Group
# Begin Group "logging"
# PROP Default_Filter ""
# Begin Group "logging_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/util/logging/logging_dmiproxy\logging_dmiproxy.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "locale"
# PROP Default_Filter ""
# Begin Group "locale_dmiproxy"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/locale/locale_dmiproxy\locale_dmiproxy.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
