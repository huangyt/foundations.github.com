# Microsoft Developer Studio Project File - Name="acdkx_rdmi" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdkx_rdmi - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_rdmi.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_rdmi.mak" CFG="acdkx_rdmi - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdkx_rdmi - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdkx_rdmi - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdkx_rdmi - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdkx_rdmi_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdkx_rdmi_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDKX_RDMI_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib ..\..\bin\acdk_net_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdkx_rdmi_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdkx_rdmi - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdkx_rdmi_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdkx_rdmi_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDKX_RDMI_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib ..\..\bin\acdk_net_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdkx_rdmi_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdkx_rdmi - Win32 Release"
# Name "acdkx_rdmi - Win32 Debug"


# Begin Group "acdkx"
# PROP Default_Filter ""
# Begin Group "rdmi"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/rdmi\BinaryProtocol.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\Config.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\Connection.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\LocalObjectRepository.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\Protocol.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\ProtocolImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\rdmi.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\RemoteDmiProxy.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\RemoteDmiServer.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\ServerImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\TcpServer.h
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\BinaryProtocol.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\LocalObjectRepository.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\rdmi_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\RemoteDmiProxy.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\RemoteDmiServer.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/rdmi\TcpServer.cpp
# End Source File
# end Group
# end Group


# End Target
# End Project
