# Microsoft Developer Studio Project File - Name="acdk_xml" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_xml - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_xml.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_xml.mak" CFG="acdk_xml - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_xml - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_xml - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_xml - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_xml_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_xml_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_XML_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib ..\..\bin\acdk_net_r.lib ..\..\bin\org_xml_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_xml_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_xml - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_xml_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_xml_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_XML_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib ..\..\bin\acdk_net_d.lib ..\..\bin\org_xml_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_xml_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_xml - Win32 Release"
# Name "acdk_xml - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "xml"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/xml\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/xml\xml.h
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLObjectReader.h
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLObjectWriter.h
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLTokenizer.h
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLObjectReader.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLObjectWriter.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml\XMLTokenizer.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml\xml_metainf_base.cpp
# End Source File
# Begin Group "sax"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/xml/sax\AttributeListImpl.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\LibXMLInternals.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\NamedBufferReader.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\sax.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\XmlLibLocator.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\XMLParser.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\XMLReader.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\sax_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\XMLParser.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/sax\XMLReaderImpl.cpp
# End Source File
# end Group
# Begin Group "dom"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/xml/dom\dom.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\DOMParser.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\NodeArrayList.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\NodeIteratorWalker.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\NodeUtil.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLAttr.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLCDATASection.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLCharacterData.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLComment.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLDocument.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLElement.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLNamedNodeMap.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLNode.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLNodeList.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLText.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\DOMParser.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\dom_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\NodeIteratorWalker.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\NodeUtil.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLDocument.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLElement.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/dom\XMLNode.cpp
# End Source File
# end Group
# Begin Group "libxmldom"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLAttr.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLCDATASection.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLCharacterData.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLComment.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocument.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocumentBuilder.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocumentFragment.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocumentType.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\libxmldom.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDOMInternals.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLElement.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLEntity.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLEntityReference.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLNamedNodeMap.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLNode.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLNodeList.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLNotation.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLPathNodeList.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLProcessingInstruction.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLText.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLXPathExpression.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLXPathNSResolver.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLXPathResult.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocument.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLDocumentBuilder.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\libxmldom_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLElement.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLText.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLXPathExpression.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/libxmldom\LibXMLXPathResult.cpp
# End Source File
# end Group
# Begin Group "parsers"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/xml/parsers\DocumentBuilder.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/parsers\DocumentBuilderFactory.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/parsers\ParserConfigurationException.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/parsers\parsers.h
# End Source File

# Begin Source File
SOURCE=acdk/xml/parsers\DocumentBuilder.cpp
# End Source File

# Begin Source File
SOURCE=acdk/xml/parsers\parsers_metainf_base.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "libxml"
# PROP Default_Filter ""

# Begin Source File
SOURCE=libxml\c14n.h
# End Source File

# Begin Source File
SOURCE=libxml\catalog.h
# End Source File

# Begin Source File
SOURCE=libxml\chvalid.h
# End Source File

# Begin Source File
SOURCE=libxml\debugXML.h
# End Source File

# Begin Source File
SOURCE=libxml\dict.h
# End Source File

# Begin Source File
SOURCE=libxml\DOCBparser.h
# End Source File

# Begin Source File
SOURCE=libxml\encoding.h
# End Source File

# Begin Source File
SOURCE=libxml\entities.h
# End Source File

# Begin Source File
SOURCE=libxml\globals.h
# End Source File

# Begin Source File
SOURCE=libxml\hash.h
# End Source File

# Begin Source File
SOURCE=libxml\HTMLparser.h
# End Source File

# Begin Source File
SOURCE=libxml\HTMLtree.h
# End Source File

# Begin Source File
SOURCE=libxml\list.h
# End Source File

# Begin Source File
SOURCE=libxml\nanoftp.h
# End Source File

# Begin Source File
SOURCE=libxml\nanohttp.h
# End Source File

# Begin Source File
SOURCE=libxml\parser.h
# End Source File

# Begin Source File
SOURCE=libxml\parserInternals.h
# End Source File

# Begin Source File
SOURCE=libxml\pattern.h
# End Source File

# Begin Source File
SOURCE=libxml\relaxng.h
# End Source File

# Begin Source File
SOURCE=libxml\SAX.h
# End Source File

# Begin Source File
SOURCE=libxml\SAX2.h
# End Source File

# Begin Source File
SOURCE=libxml\schemasInternals.h
# End Source File

# Begin Source File
SOURCE=libxml\threads.h
# End Source File

# Begin Source File
SOURCE=libxml\tree.h
# End Source File

# Begin Source File
SOURCE=libxml\uri.h
# End Source File

# Begin Source File
SOURCE=libxml\valid.h
# End Source File

# Begin Source File
SOURCE=libxml\xinclude.h
# End Source File

# Begin Source File
SOURCE=libxml\xlink.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlautomata.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlerror.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlexports.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlIO.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlmemory.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlmodule.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlreader.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlregexp.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlsave.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlschemas.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlschemastypes.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlstring.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlunicode.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlversion.h
# End Source File

# Begin Source File
SOURCE=libxml\xmlwriter.h
# End Source File

# Begin Source File
SOURCE=libxml\xpath.h
# End Source File

# Begin Source File
SOURCE=libxml\xpathInternals.h
# End Source File

# Begin Source File
SOURCE=libxml\xpointer.h
# End Source File
# end Group
# Begin Group "expat"
# PROP Default_Filter ""

# Begin Source File
SOURCE=expat/xmlparse.c
# End Source File

# Begin Source File
SOURCE=expat/xmlrole.c
# End Source File

# Begin Source File
SOURCE=expat/xmltok.c
# End Source File
# end Group
# Begin Group "libxml2"
# PROP Default_Filter ""

# Begin Source File
SOURCE=libxml2/c14n.c
# End Source File

# Begin Source File
SOURCE=libxml2/catalog.c
# End Source File

# Begin Source File
SOURCE=libxml2/chvalid.c
# End Source File

# Begin Source File
SOURCE=libxml2/debugXML.c
# End Source File

# Begin Source File
SOURCE=libxml2/dict.c
# End Source File

# Begin Source File
SOURCE=libxml2/DOCBparser.c
# End Source File

# Begin Source File
SOURCE=libxml2/encoding.c
# End Source File

# Begin Source File
SOURCE=libxml2/entities.c
# End Source File

# Begin Source File
SOURCE=libxml2/error.c
# End Source File

# Begin Source File
SOURCE=libxml2/globals.c
# End Source File

# Begin Source File
SOURCE=libxml2/hash.c
# End Source File

# Begin Source File
SOURCE=libxml2/HTMLparser.c
# End Source File

# Begin Source File
SOURCE=libxml2/HTMLtree.c
# End Source File

# Begin Source File
SOURCE=libxml2/legacy.c
# End Source File

# Begin Source File
SOURCE=libxml2/list.c
# End Source File

# Begin Source File
SOURCE=libxml2/nanoftp.c
# End Source File

# Begin Source File
SOURCE=libxml2/nanohttp.c
# End Source File

# Begin Source File
SOURCE=libxml2/parser.c
# End Source File

# Begin Source File
SOURCE=libxml2/parserInternals.c
# End Source File

# Begin Source File
SOURCE=libxml2/pattern.c
# End Source File

# Begin Source File
SOURCE=libxml2/relaxng.c
# End Source File

# Begin Source File
SOURCE=libxml2/SAX.c
# End Source File

# Begin Source File
SOURCE=libxml2/SAX2.c
# End Source File

# Begin Source File
SOURCE=libxml2/threads.c
# End Source File

# Begin Source File
SOURCE=libxml2/tree.c
# End Source File

# Begin Source File
SOURCE=libxml2/triostr.c
# End Source File

# Begin Source File
SOURCE=libxml2/uri.c
# End Source File

# Begin Source File
SOURCE=libxml2/valid.c
# End Source File

# Begin Source File
SOURCE=libxml2/xinclude.c
# End Source File

# Begin Source File
SOURCE=libxml2/xlink.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlIO.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlmemory.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlreader.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlregexp.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlsave.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlschemas.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlschemastypes.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlstring.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlunicode.c
# End Source File

# Begin Source File
SOURCE=libxml2/xmlwriter.c
# End Source File

# Begin Source File
SOURCE=libxml2/xpath.c
# End Source File

# Begin Source File
SOURCE=libxml2/xpointer.c
# End Source File
# end Group


# End Target
# End Project
