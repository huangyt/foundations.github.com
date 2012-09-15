# Microsoft Developer Studio Project File - Name="acdk_make" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_make - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_make.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_make.mak" CFG="acdk_make - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_make - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_make - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_make - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_make\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_make\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_MAKE_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_cfgscript_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_make_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_make - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_make\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_make\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_MAKE_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_cfgscript_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_make_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_make - Win32 Release"
# Name "acdk_make - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "make"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/make\AbstractCppTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AbstractTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkBinaryTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkExeTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkLibTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkMetainfLibTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkProjectTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkTestExeTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkUnitConfigTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\ChDir.h
# End Source File

# Begin Source File
SOURCE=acdk/make\CommandLineCompilerTool.h
# End Source File

# Begin Source File
SOURCE=acdk/make\CompileTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/make\CppSourceDependTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\DirExistsTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\FileCopyTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\FileDeleteTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\FileDependTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\FileOpTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\FileSet.h
# End Source File

# Begin Source File
SOURCE=acdk/make\JavaCompileTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\JobExecuterTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\JobTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\LookupFileTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\make.h
# End Source File

# Begin Source File
SOURCE=acdk/make\MakeProps.h
# End Source File

# Begin Source File
SOURCE=acdk/make\PlattformSelectTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\ProjectTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\ScriptExecuteTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\ShellExecuteTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\Task.h
# End Source File

# Begin Source File
SOURCE=acdk/make\TaskCfgFile.h
# End Source File

# Begin Source File
SOURCE=acdk/make\TaskInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/make\TaskManager.h
# End Source File

# Begin Source File
SOURCE=acdk/make\TestTask.h
# End Source File

# Begin Source File
SOURCE=acdk/make\ThreadPool.h
# End Source File

# Begin Source File
SOURCE=acdk/make\Tool.h
# End Source File

# Begin Source File
SOURCE=acdk/make\AbstractCppTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AbstractTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkBinaryTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkExeTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkLibTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkMetainfLibTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkProjectTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkTestExeTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\AcdkUnitConfigTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\CommandLineCompilerTool.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\CompileTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\CppSourceDependTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\FileCopyTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\FileDeleteTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\FileDependTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\FileOpTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\FileSet.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\JavaCompileTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\JobExecuterTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\JobTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\LookupFileTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\MakeProps.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\make_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\PlattformSelectTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\ProjectTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\ScriptExecuteTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\ShellExecuteTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\Task.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\TaskCfgFile.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\TaskManager.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\TestTask.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\ThreadPool.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make\Tool.cpp
# End Source File
# Begin Group "make_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/make/make_metainf\make_metainf_ext.cpp
# End Source File

# Begin Source File
SOURCE=acdk/make/make_metainf\make_metainf_ext1.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
