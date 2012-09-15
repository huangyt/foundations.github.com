# Microsoft Developer Studio Project File - Name="acdk_wx_metainf" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_wx_metainf - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_wx_metainf.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_wx_metainf.mak" CFG="acdk_wx_metainf - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_wx_metainf - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_wx_metainf - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_wx_metainf - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_wx_metainf\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_wx_metainf\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_wx_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_wx_metainf_r.dll"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_RELEASE)
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_wx_metainf - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_wx_metainf\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_wx_metainf\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_wx_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_wx_metainf_d.dll"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_DEBUG)

!ENDIF 

# Begin Target

# Name "acdk_wx_metainf - Win32 Release"
# Name "acdk_wx_metainf - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "wx"
# PROP Default_Filter ""
# Begin Group "wx_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext1.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext2.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext3.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext4.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext5.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext6.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext7.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext8.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx/wx_metainf\wx_metainf_ext9.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
