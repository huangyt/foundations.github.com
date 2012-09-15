# Microsoft Developer Studio Project File - Name="acdk_aci" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_aci - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_aci.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_aci.mak" CFG="acdk_aci - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_aci - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_aci - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_aci - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_aci\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_aci\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /GX /O2  /D "OS_WIN32" /D "IN_ACDK_ACI_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "_USRDLL" /I "."  /I "../../include"  /Zm400 /c
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
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib ..\..\bin\acdk_cfgscript_r.lib ..\..\bin\acdk_text_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_aci_r.dll" ""
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_aci - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_aci\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_aci\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_ACI_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "../../include"  /FR /Zm400 /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib ..\..\bin\acdk_cfgscript_d.lib ..\..\bin\acdk_text_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_aci_d.dll" ""

!ENDIF 

# Begin Target

# Name "acdk_aci - Win32 Release"
# Name "acdk_aci - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "aci"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci\AccLoader.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\aci.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\aci_metainf.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\ClazzSymbolTable.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\Compiler.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\core_stack.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\DClazzInfo.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\StdAci.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\SymbolTable.h
# End Source File

# Begin Source File
SOURCE=acdk/aci\aci_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci\ClazzSymbolTable.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci\Compiler.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci\DClazzInfo.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci\DClazzInfoPersist.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci\SymbolTable.cpp
# End Source File
# Begin Group "ast"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci/ast\AciCodeAttributeData.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\ast.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\AstNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\AstNodeVisitor.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\ast_metainf.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\EofTerminal.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Expression.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Identifier.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Keyword.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Literal.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Statement.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Terminal.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Whitespace.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\AstNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\ast_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/ast\Terminal.cpp
# End Source File
# end Group
# Begin Group "parser"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci/parser\BlockCommentParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\DecimalLiteralParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\FloatLiteralParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\IdentifierParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\KeywordParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\LineCommentParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\LiteralParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\ParseEnv.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\ParseException.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\ParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\parser.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\RegScanParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\Scanner.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\StringTerminalParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\SyntaxNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\SyntaxParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\TerminalParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\WhiteSpaceParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\BlockCommentParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\DecimalLiteralParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\FloatLiteralParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\IdentifierParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\KeywordParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\ParseEnv.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\ParseException.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\parser_metainf_base.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\RegScanParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\Scanner.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\StringTerminalParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\SyntaxNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\SyntaxParseNode.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/parser\TerminalParseNode.cpp
# End Source File
# end Group
# Begin Group "vm"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci/vm\EvalEnv.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\Executable.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\OpCodeOp.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\OpCodeStm.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\vm.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\EvalEnv.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\ExecutableCollector.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\OpCodeStm.cpp
# End Source File

# Begin Source File
SOURCE=acdk/aci/vm\vm_metainf_base.cpp
# End Source File
# end Group
# Begin Group "util"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci/util\CodeLocation.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/util\Source.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/util\StringSource.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/util\TStackedSet.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/util\util.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/util\util_metainf_base.cpp
# End Source File
# end Group
# Begin Group "sasm"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/aci/sasm\AsmParseNode.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/sasm\sasm.h
# End Source File

# Begin Source File
SOURCE=acdk/aci/sasm\AsmParseNode.cpp
# End Source File
# end Group
# end Group
# end Group


# End Target
# End Project
