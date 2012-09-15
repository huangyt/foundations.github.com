# Microsoft Developer Studio Project File - Name="acdkx_orb" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdkx_orb - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_orb.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_orb.mak" CFG="acdkx_orb - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdkx_orb - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdkx_orb - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdkx_orb - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdkx_orb\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdkx_orb\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDKX_ORB_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_net_r.lib ..\..\bin\acdk_text_r.lib ..\..\bin\org_xml_r.lib ..\..\bin\acdk_xml_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdkx_orb_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdkx_orb - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdkx_orb\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdkx_orb\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDKX_ORB_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_net_d.lib ..\..\bin\acdk_text_d.lib ..\..\bin\org_xml_d.lib ..\..\bin\acdk_xml_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdkx_orb_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdkx_orb - Win32 Release"
# Name "acdkx_orb - Win32 Debug"


# Begin Group "org"
# PROP Default_Filter ""
# Begin Group "omg"
# PROP Default_Filter ""
# Begin Group "CORBA"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA\Any.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\AttributeDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Contained.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Container.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Context.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\CORBA.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\ExceptionDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\IDLType.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\InterfaceDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\IRObject.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\ModuleDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Object.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\OperationDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\ORB.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\OrbExceptions.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\PrimitiveDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Repository.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\SequenceDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\ServerRequest.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\StringDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\StructDef.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\TypeCode.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Any.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Contained.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\Container.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\CORBA_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\InterfaceDef.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\ORB.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA\OrbExceptions.cpp
# End Source File
# Begin Group "CORBA_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/CORBA_metainf\CORBA_metainf_ext.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/CORBA_metainf\CORBA_metainf_ext1.cpp
# End Source File
# end Group
# Begin Group "GIOP"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/GIOP\GIOP.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/GIOP\GIOP.cpp
# End Source File
# end Group
# Begin Group "IIOP"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/IIOP\IIOP.h
# End Source File
# end Group
# Begin Group "IOP"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/IOP\IOP.h
# End Source File
# end Group
# Begin Group "portable"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/portable\ApplicationException.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\Delegate.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\InputStream.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\InvokeHandler.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\ObjectImpl.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\OutputStream.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\portable.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\RemarshalException.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\ResponseHandler.h
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\ObjectImpl.cpp
# End Source File

# Begin Source File
SOURCE=org/omg/CORBA/portable\portable_metainf_base.cpp
# End Source File
# Begin Group "portable_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CORBA/portable/portable_metainf\portable_metainf_ext.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "CosNaming"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CosNaming\Config.h
# End Source File

# Begin Source File
SOURCE=org/omg/CosNaming\CosNaming.h
# End Source File

# Begin Source File
SOURCE=org/omg/CosNaming\CosNaming_metainf_base.cpp
# End Source File
# Begin Group "CosNaming_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/CosNaming/CosNaming_metainf\CosNaming_metainf_ext.cpp
# End Source File
# end Group
# end Group
# Begin Group "PortableServer"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/PortableServer\POA.h
# End Source File

# Begin Source File
SOURCE=org/omg/PortableServer\POAManager.h
# End Source File

# Begin Source File
SOURCE=org/omg/PortableServer\PortableServer.h
# End Source File

# Begin Source File
SOURCE=org/omg/PortableServer\PortableServer_metainf_base.cpp
# End Source File
# Begin Group "PortableServer_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/omg/PortableServer/PortableServer_metainf\PortableServer_metainf_ext.cpp
# End Source File
# end Group
# end Group
# end Group
# end Group
# Begin Group "acdkx"
# PROP Default_Filter ""
# Begin Group "orb"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/orb\acdk2orb.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AcdkObject.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AcdkObjectInterface.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AContextImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AORB.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AServerRequestImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CDRObjectReader.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CDRObjectWriter.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\Config.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CorObject.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\GIOPMessage.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ObjectKey.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\orb.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbConnection.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbConnectionMgr.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbInputStream.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbOutputStream.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ReplyInputStream.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ReplyOutputStream.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\RequestOutputStream.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ServerDelegate.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\std_orb.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb\acdk2orb.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AcdkObject.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AcdkObject_clazzInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AORB.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\AServerRequestImpl.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CDRObjectReader.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CDRObjectWriter.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CorObject.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\CorObject_clazzInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\GIOPMessage.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ObjectKey.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbConnectionMgr.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\OrbOutputStream.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\orb_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ReplyInputStream.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ReplyOutputStream.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\RequestOutputStream.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/orb\ServerDelegate.cpp
# End Source File
# Begin Group "orb_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/orb/orb_metainf\orb_metainf_ext.cpp
# End Source File
# end Group
# Begin Group "selftests"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/orb/selftests\selftests.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb/selftests\TestInterface.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb/selftests\TestInterfaceImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/orb/selftests\selftests_metainf_base.cpp
# End Source File
# Begin Group "selftests_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/orb/selftests/selftests_metainf\selftests_metainf_ext.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "arb"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/arb\ADelegate.h
# End Source File

# Begin Source File
SOURCE=acdkx/arb\AObjectImpl.h
# End Source File

# Begin Source File
SOURCE=acdkx/arb\arb.h
# End Source File

# Begin Source File
SOURCE=acdkx/arb\Config.h
# End Source File

# Begin Source File
SOURCE=acdkx/arb\XMLDelegate.h
# End Source File

# Begin Source File
SOURCE=acdkx/arb\AObjectImpl.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/arb\arb.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/arb\arb_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdkx/arb\XMLDelegate.cpp
# End Source File
# Begin Group "arb_metainf"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdkx/arb/arb_metainf\arb_metainf_ext.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
