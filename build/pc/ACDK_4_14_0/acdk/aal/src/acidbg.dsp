# Microsoft Developer Studio Project File - Name="acidbg" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=acidbg - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acidbg.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acidbg.mak" CFG="acidbg - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acidbg - Win32 Release" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE "acidbg - Win32 Debug" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acidbg - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acidbg_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acidbg_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /GX /O2  /D "OS_WIN32"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /Zm400 /c
# SUBTRACT CPP /Z<none>
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_cfgscript_r.lib ..\..\bin\acdk_aci_r.lib ..\..\bin\acdk_aci_guidbg_r.lib ..\..\bin\acdk_wx_r.lib /nologo /debug /debugtype:coff /machine:I386  /out:"..\..\bin\acidbg_r.exe"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_RELEASE)
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acidbg - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acidbg_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acidbg_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od  /D "OS_WIN32"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /FR /Zm400 /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_cfgscript_d.lib ..\..\bin\acdk_aci_d.lib ..\..\bin\acdk_aci_guidbg_d.lib ..\..\bin\acdk_wx_d.lib /nologo /debug /pdbtype:sept /machine:I386  /out:"..\..\bin\acidbg_d.exe"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_DEBUG)

!ENDIF 

# Begin Target

# Name "acidbg - Win32 Release"
# Name "acidbg - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "tools"
# PROP Default_Filter ""
# Begin Group "acidbg"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/tools/acidbg\AciDbgFrame.h
# End Source File

# Begin Source File
SOURCE=acdk/tools/acidbg\acidbg.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group ".."
# PROP Default_Filter ""
# Begin Group ".."
# PROP Default_Filter ""
# Begin Group "acdk_core"
# PROP Default_Filter ""
# Begin Group "src"
# PROP Default_Filter ""
# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "res"
# PROP Default_Filter ""

# Begin Source File
SOURCE=../../acdk_core/src/acdk/res/wx.rc
# End Source File
# end Group
# end Group
# end Group
# end Group
# end Group
# end Group


# End Target
# End Project
