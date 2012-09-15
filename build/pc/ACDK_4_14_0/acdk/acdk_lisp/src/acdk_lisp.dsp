# Microsoft Developer Studio Project File - Name="acdk_lisp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_lisp - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_lisp.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_lisp.mak" CFG="acdk_lisp - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_lisp - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_lisp - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_lisp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_lisp\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_lisp\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_LISP_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_lisp_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_lisp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_lisp\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_lisp\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_LISP_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_lisp_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_lisp - Win32 Release"
# Name "acdk_lisp - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "lisp"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lisp\acdk_lisp_decl.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\Function.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\lisp.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispArray.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispAtom.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispBinaryCode.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispBuildInFunction.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispCallBack.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispClass.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispCode.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispDmiClient.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispEnvironment.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispException.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispFunction.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispInterpreter.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispList.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispObject.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispSymbol.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispTemplateFilter.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispTokenizer.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispVar.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\StackFrame.h
# End Source File

# Begin Source File
SOURCE=acdk/lisp\Lisp.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispBinaryCode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispCallBack.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispClass.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispDmiClient.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispEnvironment.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispException.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispFunction.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispList.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispObject.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispObject_clazzinfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispTemplateFilter.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispTokenizer.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\LispVar.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp\lisp_metainf_base.cpp
# End Source File
# Begin Group "lisp_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/lisp/lisp_metainf\lisp_metainf_ext.cpp
# End Source File

# Begin Source File
SOURCE=acdk/lisp/lisp_metainf\lisp_metainf_ext1.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
