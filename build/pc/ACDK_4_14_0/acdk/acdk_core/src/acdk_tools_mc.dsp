# Microsoft Developer Studio Project File - Name="acdk_tools_mc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_tools_mc - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_tools_mc.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_tools_mc.mak" CFG="acdk_tools_mc - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_tools_mc - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_tools_mc - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_tools_mc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_tools_mc\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_tools_mc\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_TOOLS_MC_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "../src"  /I "../include"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_cfgscript_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_tools_mc_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_tools_mc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_tools_mc\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_tools_mc\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_TOOLS_MC_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "../src"  /I "../include"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_cfgscript_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_tools_mc_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_tools_mc - Win32 Release"
# Name "acdk_tools_mc - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "tools"
# PROP Default_Filter ""
# Begin Group "mc"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/tools/mc\ArgumentInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClassInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClassInitAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClazzFlagAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClazzNameAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\CMCException.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\CodeAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\CodeInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DispatchForwardAttributeTest.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyGenerator.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyGeneratorExt.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\EnumArgAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\EnumInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\FieldInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\InvokeForwardAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\mc.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\McConfigAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MetaCompiler.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MethodAltNameAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MethodInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ModuleInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\SetDispatchAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\StringTagAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\SuperInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ThrowableAttribute.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\TokenStack.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\TypeScope.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\UnitInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ArgumentInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClassInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClassInitAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClazzFlagAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ClazzNameAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\CodeAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\CodeInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyGenerator.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\DmiProxyGeneratorExt.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\EnumArgAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\EnumInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\FieldInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\InvokeForwardAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\McConfigAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\mc_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MetaCompiler.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MethodAltNameAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\MethodInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ModuleInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\SetDispatchAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\StringTagAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\SuperInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\ThrowableAttribute.cpp
# End Source File

# Begin Source File
SOURCE=acdk/tools/mc\TypeScope.cpp
# End Source File
# Begin Group "mc_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/tools/mc/mc_metainf\mc_metainf_ext.cpp
# End Source File
# end Group
# end Group
# end Group
# end Group


# End Target
# End Project
