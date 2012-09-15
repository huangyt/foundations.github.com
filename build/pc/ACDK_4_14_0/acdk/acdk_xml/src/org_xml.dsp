# Microsoft Developer Studio Project File - Name="org_xml" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=org_xml - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "org_xml.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "org_xml.mak" CFG="org_xml - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "org_xml - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "org_xml - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "org_xml - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\org_xml\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\org_xml\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ORG_XML_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_text_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\org_xml_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "org_xml - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\org_xml\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\org_xml\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ORG_XML_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_text_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\org_xml_d.dll" ""

!ENDIF 

# Begin Target

# Name "org_xml - Win32 Release"
# Name "org_xml - Win32 Debug"


# Begin Group "org"
# PROP Default_Filter ""
# Begin Group "xml"
# PROP Default_Filter ""
# Begin Group "sax"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/xml/sax\AttributeList.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\Attributes.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\Config.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\ContentHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\DeclHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\DocumentHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\DTDHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\EntityResolver.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\ErrorHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\HandlerBase.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\InputSource.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\LexicalHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\Locator.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\Parser.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\sax.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\SAXException.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\SAXNotRecognizedException.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\SAXNotSupportedException.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\SAXParseException.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\XMLFilter.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\XMLReader.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax\sax_metainf_base.cpp
# End Source File
# Begin Group "helpers"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/xml/sax/helpers\AttributesImpl.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\CollectiveErrorHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\DefaultHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\helpers.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\LocatorImpl.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\MiscXmlUtils.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\NamespaceSupport.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\StdErrorHandler.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\XMLFilterImpl.h
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\AttributesImpl.cpp
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\helpers_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\MiscXmlUtils.cpp
# End Source File

# Begin Source File
SOURCE=org/xml/sax/helpers\NamespaceSupport.cpp
# End Source File
# end Group
# end Group
# end Group
# Begin Group "w3c"
# PROP Default_Filter ""
# Begin Group "dom"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/w3c/dom\Attr.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Branch.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\CDATASection.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\CharacterData.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Comment.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Document.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DocumentFragment.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DocumentType.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\dom.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMConfiguration.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMError.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMErrorHandler.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMException.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMImplementation.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMLocator.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMWriter.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Element.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Entity.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\EntityReference.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\NamedNodeMap.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Node.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\NodeList.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Notation.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\ProcessingInstruction.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Text.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\DOMWriter.cpp
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\dom_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=org/w3c/dom\Node.cpp
# End Source File
# Begin Group "xpath"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/w3c/dom/xpath\xpath.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathEvaluator.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathException.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathExpression.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathNamespace.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathNSResolver.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\XPathResult.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/xpath\xpath_metainf_base.cpp
# End Source File
# end Group
# Begin Group "traversal"
# PROP Default_Filter ""

# Begin Source File
SOURCE=org/w3c/dom/traversal\DocumentTraversal.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/traversal\NodeFilter.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/traversal\NodeIterator.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/traversal\traversal.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/traversal\TreeWalker.h
# End Source File

# Begin Source File
SOURCE=org/w3c/dom/traversal\traversal_metainf_base.cpp
# End Source File
# end Group
# end Group
# end Group
# end Group


# End Target
# End Project
