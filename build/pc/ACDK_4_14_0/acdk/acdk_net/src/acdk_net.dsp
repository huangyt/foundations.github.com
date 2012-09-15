# Microsoft Developer Studio Project File - Name="acdk_net" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_net - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_net.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_net.mak" CFG="acdk_net - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_net - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_net - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_net - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_net\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_net\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_NET_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_net_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_net - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_net\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_net\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_NET_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_net_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_net - Win32 Release"
# Name "acdk_net - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "net"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/net\Authenticator.h
# End Source File

# Begin Source File
SOURCE=acdk/net\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/net\ContentHandler.h
# End Source File

# Begin Source File
SOURCE=acdk/net\ContentHandlerFactory.h
# End Source File

# Begin Source File
SOURCE=acdk/net\DatagramPacket.h
# End Source File

# Begin Source File
SOURCE=acdk/net\DatagramSocket.h
# End Source File

# Begin Source File
SOURCE=acdk/net\FileNameMap.h
# End Source File

# Begin Source File
SOURCE=acdk/net\FileURLConnection.h
# End Source File

# Begin Source File
SOURCE=acdk/net\HeaderFieldHelper.h
# End Source File

# Begin Source File
SOURCE=acdk/net\HttpURLConnection.h
# End Source File

# Begin Source File
SOURCE=acdk/net\HttpURLConnectionImpl.h
# End Source File

# Begin Source File
SOURCE=acdk/net\InetAddress.h
# End Source File

# Begin Source File
SOURCE=acdk/net\InetSocketAddress.h
# End Source File

# Begin Source File
SOURCE=acdk/net\InetURLConnection.h
# End Source File

# Begin Source File
SOURCE=acdk/net\MalformedURLException.h
# End Source File

# Begin Source File
SOURCE=acdk/net\MimeTypeMapper.h
# End Source File

# Begin Source File
SOURCE=acdk/net\net.h
# End Source File

# Begin Source File
SOURCE=acdk/net\netsysincl.h
# End Source File

# Begin Source File
SOURCE=acdk/net\PasswordAuthentication.h
# End Source File

# Begin Source File
SOURCE=acdk/net\ProtocolException.h
# End Source File

# Begin Source File
SOURCE=acdk/net\ServerSocket.h
# End Source File

# Begin Source File
SOURCE=acdk/net\Socket.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketAddress.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketException.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketImpl.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketImplFactory.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketLogConsumer.h
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketOptions.h
# End Source File

# Begin Source File
SOURCE=acdk/net\TCPSocket.h
# End Source File

# Begin Source File
SOURCE=acdk/net\TCPSocketFactory.h
# End Source File

# Begin Source File
SOURCE=acdk/net\TransRateReader.h
# End Source File

# Begin Source File
SOURCE=acdk/net\TransRateWriter.h
# End Source File

# Begin Source File
SOURCE=acdk/net\UnknownServiceException.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URL.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLConnection.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLDecoder.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLEncoder.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLEncoding.h
# End Source File

# Begin Source File
SOURCE=acdk/net\UrlFileSystem.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLInterface.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLStreamHandler.h
# End Source File

# Begin Source File
SOURCE=acdk/net\URLStreamHandlerFactory.h
# End Source File

# Begin Source File
SOURCE=acdk/net\acdk_net_Package.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\Authenticator.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\DatagramSocket.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\FileURLConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\HeaderFieldHelper.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\HttpURLConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\HttpURLConnectionImpl.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\InetAddress.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\InetURLConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\MimeTypeMapper.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\net_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\ServerSocket.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\Socket.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketException.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketImpl.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketImplFactory.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketLogConsumer.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\SocketOptions.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\TCPSocket.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\TCPSocketFactory.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\TransRateReader.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\TransRateWriter.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URL.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLConnection.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLDecoder.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLEncoder.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLEncoding.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\UrlFileSystem.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLStreamHandler.cpp
# End Source File

# Begin Source File
SOURCE=acdk/net\URLStreamHandlerFactory.cpp
# End Source File
# end Group
# end Group


# End Target
# End Project
