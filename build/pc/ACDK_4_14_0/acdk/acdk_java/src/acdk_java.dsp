# Microsoft Developer Studio Project File - Name="acdk_java" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_java - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_java.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_java.mak" CFG="acdk_java - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_java - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_java - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_java - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_java\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_java\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_JAVA_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "$(ACDK_JAVA_INCLUDE)"  /I "$(ACDK_JAVA_INCLUDE_WIN32)"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib $(ACDK_JAVA_JVM_LIB).lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_java_r.dll"  /libpath:"$(ACDK_JAVA_LIBDIR)"
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_java - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_java\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_java\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_JAVA_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "$(ACDK_JAVA_INCLUDE)"  /I "$(ACDK_JAVA_INCLUDE_WIN32)"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib $(ACDK_JAVA_JVM_LIB).lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_java_d.dll"  /libpath:"$(ACDK_JAVA_LIBDIR)"

!ENDIF 

# Begin Target

# Name "acdk_java - Win32 Release"
# Name "acdk_java - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "java"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/java\acdk2java.h
# End Source File

# Begin Source File
SOURCE=acdk/java\acdk_java_AcdkObject.h
# End Source File

# Begin Source File
SOURCE=acdk/java\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/java\JavaInterpreter.h
# End Source File

# Begin Source File
SOURCE=acdk/java\JavaObject.h
# End Source File

# Begin Source File
SOURCE=acdk/java\jniext.h
# End Source File

# Begin Source File
SOURCE=acdk/java\acdk2java.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java\acdk_java_AcdkObject.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java\JavaInterpreter.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java\JavaObject.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java\JavaObject_clazzinfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java\jniext.cpp
# End Source File
# Begin Group "awt"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/java/awt\AWTEvent.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt\AWTEvent.cpp
# End Source File
# Begin Group "event"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/java/awt/event\acdk_java_awt_event_AwtListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\ActionEvent.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\ActionListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\AWTEventListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\AwtListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\KeyEvent.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\KeyListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\MouseEvent.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\MouseListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\WindowEvent.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\WindowListener.h
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\acdk_java_awt_event_AwtListener.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\ActionEvent.cpp
# End Source File

# Begin Source File
SOURCE=acdk/java/awt/event\KeyEvent.cpp
# End Source File
# end Group
# end Group
# end Group
# end Group


# End Target
# End Project
