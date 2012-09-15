# Microsoft Developer Studio Project File - Name="acdk_lang_sys_Allocator_Test" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=acdk_lang_sys_Allocator_Test - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_lang_sys_Allocator_Test.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_lang_sys_Allocator_Test.mak" CFG="acdk_lang_sys_Allocator_Test - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_lang_sys_Allocator_Test - Win32 Release" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE "acdk_lang_sys_Allocator_Test - Win32 Debug" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_lang_sys_Allocator_Test - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\..\..\..\bin"
# PROP BASE Intermediate_Dir "obj\acdk_lang_sys_Allocator_Test\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\..\bin"
# PROP Intermediate_Dir "obj\acdk_lang_sys_Allocator_Test\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /Zm200 /c
# ADD CPP /nologo /MD /Gi /GR /GX /O2 /I "../src" /I "../include" /I "../../../../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /c
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\..\bin\acdk_core_dll_r.lib /nologo /machine:I386 /out:"..\..\..\..\..\bin\acdk_lang_sys_Allocator_Test_r.exe"

!ELSEIF  "$(CFG)" == "acdk_lang_sys_Allocator_Test - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\..\..\..\bin"
# PROP BASE Intermediate_Dir "obj\acdk_lang_sys_Allocator_Test\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\..\bin"
# PROP Intermediate_Dir "obj\acdk_lang_sys_Allocator_Test\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /Zm200 /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od /I "../src" /I "../include" /I "../../../../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /FR /c
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\..\bin\acdk_core_dll_d.lib /nologo /debug /machine:I386 /out:"..\..\..\..\..\bin\acdk_lang_sys_Allocator_Test_d.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "acdk_lang_sys_Allocator_Test - Win32 Release"
# Name "acdk_lang_sys_Allocator_Test - Win32 Debug"
# Begin Source File

SOURCE=acdk_lang_sys_Allocator_Test.cpp
DEP_CPP_ACDK_=\
	"..\..\..\..\..\include\acdk.h"\
	"..\..\..\..\..\include\acdk\Compiler.h"\
	"..\..\..\..\..\include\acdk\Config.h"\
	"..\..\..\..\..\include\acdk\io\AbstractFilterReader.h"\
	"..\..\..\..\..\include\acdk\io\AbstractFilterWriter.h"\
	"..\..\..\..\..\include\acdk\io\AbstractReader.h"\
	"..\..\..\..\..\include\acdk\io\AbstractWriter.h"\
	"..\..\..\..\..\include\acdk\io\FilterReader.h"\
	"..\..\..\..\..\include\acdk\io\FilterWriter.h"\
	"..\..\..\..\..\include\acdk\io\InputReader.h"\
	"..\..\..\..\..\include\acdk\io\io.h"\
	"..\..\..\..\..\include\acdk\io\IOException.h"\
	"..\..\..\..\..\include\acdk\io\PrintWriter.h"\
	"..\..\..\..\..\include\acdk\io\Reader.h"\
	"..\..\..\..\..\include\acdk\io\Serializable.h"\
	"..\..\..\..\..\include\acdk\io\Writer.h"\
	"..\..\..\..\..\include\acdk\lang\ArrayIndexOutOfBoundsException.h"\
	"..\..\..\..\..\include\acdk\lang\Class.h"\
	"..\..\..\..\..\include\acdk\lang\ClazzInfo.h"\
	"..\..\..\..\..\include\acdk\lang\Cloneable.h"\
	"..\..\..\..\..\include\acdk\lang\Comparable.h"\
	"..\..\..\..\..\include\acdk\lang\Exception.h"\
	"..\..\..\..\..\include\acdk\lang\IllegalAccessException.h"\
	"..\..\..\..\..\include\acdk\lang\IndexOutOfBoundsException.h"\
	"..\..\..\..\..\include\acdk\lang\InOutPreDeclaration.h"\
	"..\..\..\..\..\include\acdk\lang\lang.h"\
	"..\..\..\..\..\include\acdk\lang\Math.h"\
	"..\..\..\..\..\include\acdk\lang\NullPointerException.h"\
	"..\..\..\..\..\include\acdk\lang\Object.h"\
	"..\..\..\..\..\include\acdk\lang\ObjectBase.h"\
	"..\..\..\..\..\include\acdk\lang\ObjectImpl.h"\
	"..\..\..\..\..\include\acdk\lang\ObjectInline.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\AccessibleObject.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\Constructor.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\Field.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\InvocationTargetException.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\Member.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\Method.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\Modifier.h"\
	"..\..\..\..\..\include\acdk\lang\reflect\reflect.h"\
	"..\..\..\..\..\include\acdk\lang\Runnable.h"\
	"..\..\..\..\..\include\acdk\lang\RuntimeException.h"\
	"..\..\..\..\..\include\acdk\lang\String.h"\
	"..\..\..\..\..\include\acdk\lang\StringBuffer.h"\
	"..\..\..\..\..\include\acdk\lang\sys\Allocator.h"\
	"..\..\..\..\..\include\acdk\lang\sys\BasicArray.h"\
	"..\..\..\..\..\include\acdk\lang\sys\BitmapPagedAllocator.h"\
	"..\..\..\..\..\include\acdk\lang\sys\ControlPushPopOnStack.h"\
	"..\..\..\..\..\include\acdk\lang\sys\core_condition.h"\
	"..\..\..\..\..\include\acdk\lang\sys\core_fastmutex.h"\
	"..\..\..\..\..\include\acdk\lang\sys\core_mutex.h"\
	"..\..\..\..\..\include\acdk\lang\sys\core_string.h"\
	"..\..\..\..\..\include\acdk\lang\sys\core_vector.h"\
	"..\..\..\..\..\include\acdk\lang\sys\Fields.h"\
	"..\..\..\..\..\include\acdk\lang\sys\HeapFrame.h"\
	"..\..\..\..\..\include\acdk\lang\sys\ObjectArrayImpl.h"\
	"..\..\..\..\..\include\acdk\lang\sys\ObjectHeap.h"\
	"..\..\..\..\..\include\acdk\lang\sys\ObjectLockPool.h"\
	"..\..\..\..\..\include\acdk\lang\sys\RefHolder.h"\
	"..\..\..\..\..\include\acdk\lang\sys\ScriptVar.h"\
	"..\..\..\..\..\include\acdk\lang\sys\sys.h"\
	"..\..\..\..\..\include\acdk\lang\sys\SysRefHolder.h"\
	"..\..\..\..\..\include\acdk\lang\sys\TraceCallStack.h"\
	"..\..\..\..\..\include\acdk\lang\System.h"\
	"..\..\..\..\..\include\acdk\lang\SystemError.h"\
	"..\..\..\..\..\include\acdk\lang\Thread.h"\
	"..\..\..\..\..\include\acdk\lang\ThreadGroup.h"\
	"..\..\..\..\..\include\acdk\lang\ThreadImpl.h"\
	"..\..\..\..\..\include\acdk\lang\ThreadLocalImpl.h"\
	"..\..\..\..\..\include\acdk\lang\Throwable.h"\
	"..\..\..\..\..\include\acdk\lang\UnsupportedOperationException.h"\
	"..\..\..\..\..\include\acdk\acdk.h"\
	"..\..\..\..\..\include\acdk\Platform.h"\
	"..\..\..\..\..\include\acdk\util\AbstractCollection.h"\
	"..\..\..\..\..\include\acdk\util\AbstractList.h"\
	"..\..\..\..\..\include\acdk\util\AbstractMap.h"\
	"..\..\..\..\..\include\acdk\util\AbstractSet.h"\
	"..\..\..\..\..\include\acdk\util\ArrayList.h"\
	"..\..\..\..\..\include\acdk\util\Arrays.h"\
	"..\..\..\..\..\include\acdk\util\BasicMapEntry.h"\
	"..\..\..\..\..\include\acdk\util\Bucket.h"\
	"..\..\..\..\..\include\acdk\util\Collection.h"\
	"..\..\..\..\..\include\acdk\util\Comparator.h"\
	"..\..\..\..\..\include\acdk\util\ConcurrentModificationException.h"\
	"..\..\..\..\..\include\acdk\util\HashMap.h"\
	"..\..\..\..\..\include\acdk\util\Iterator.h"\
	"..\..\..\..\..\include\acdk\util\Map.h"\
	"..\..\..\..\..\include\acdk\util\NoSuchElementException.h"\
	"..\..\..\..\..\include\acdk\util\Properties.h"\
	"..\..\..\..\..\include\acdk\util\Set.h"\
	"..\..\..\..\..\include\acdk\util\util.h"\
	"..\..\..\..\..\include\acdk\util\Vector.h"\
	{$(INCLUDE)}"pthread.h"\
	{$(INCLUDE)}"sys\errno.h"\
	
# End Source File
# End Target
# End Project
