# Microsoft Developer Studio Project File - Name="acdk_aal_Test" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=acdk_aal_Test - Win32 Debug
!MESSAGE Dies ist kein g�ltiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und f�hren Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_aal_Test.mak".
!MESSAGE 
!MESSAGE Sie k�nnen beim Ausf�hren von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_aal_Test.mak" CFG="acdk_aal_Test - Win32 Debug"
!MESSAGE 
!MESSAGE F�r die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_aal_Test - Win32 Release" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE "acdk_aal_Test - Win32 Debug" (basierend auf  "Win32 (x86) Console Application" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_aal_Test - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\..\..\bin"
# PROP BASE Intermediate_Dir ".\tobj\acdk_aal_Test\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\bin"
# PROP Intermediate_Dir ".\tobj\acdk_aal_Test\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /GX /O2  /D "OS_WIN32"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /I "../src"  /I "../include"  /I "../../../../include"  /Zm400 /c
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
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\bin\acdk_core_r.lib ..\..\..\..\bin\acdk_tools_aunit_r.lib ..\..\..\..\bin\acdk_text_r.lib ..\..\..\..\bin\acdk_aal_r.lib /nologo /debug /debugtype:coff /machine:I386  /out:"..\..\..\..\bin\acdk_aal_Test_r.exe" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_aal_Test - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\..\..\bin"
# PROP BASE Intermediate_Dir ".\tobj\acdk_aal_Test\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\bin"
# PROP Intermediate_Dir ".\tobj\acdk_aal_Test\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od  /D "OS_WIN32"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /I "../src"  /I "../include"  /I "../../../../include"  /FR /Zm400 /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\bin\acdk_core_d.lib ..\..\..\..\bin\acdk_tools_aunit_d.lib ..\..\..\..\bin\acdk_text_d.lib ..\..\..\..\bin\acdk_aal_d.lib /nologo /debug /pdbtype:sept /machine:I386  /out:"..\..\..\..\bin\acdk_aal_Test_d.exe" ""

!ENDIF 

# Begin Target

# Name "acdk_aal_Test - Win32 Release"
# Name "acdk_aal_Test - Win32 Debug"


# Begin Group "."
# PROP Default_Filter ""

# Begin Source File
SOURCE=.\acdk_aal_AalObject_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_AritmExpr_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Arrays_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Basics_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_BinaryExpr_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Closure_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Compiler_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_DefunStatements_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_DeriveObject_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Exceptions_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_ExtendClass_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Functions_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Interpret_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_LiteralWrapping_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_LogicalExpr_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Operator_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_ParameterPassing_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_RelExpr_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Scanner_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Statements_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_Test.cpp
# End Source File

# Begin Source File
SOURCE=.\acdk_aal_WeakInvoke_Test.cpp
# End Source File
# end Group


# End Target
# End Project
