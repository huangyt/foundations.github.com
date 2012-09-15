# Microsoft Developer Studio Project File - Name="acdk_doc_urlify" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=acdk_doc_urlify - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_doc_urlify.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_doc_urlify.mak" CFG="acdk_doc_urlify - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_doc_urlify - Win32 Release" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE "acdk_doc_urlify - Win32 Debug" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_doc_urlify - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "../../bin"
# PROP BASE Intermediate_Dir "../obj/acdk_doc_urlify/dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../bin"
# PROP Intermediate_Dir "../obj/acdk_doc_urlify/dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi /GR /GX /O2 /I "../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /Zm400 /c
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ../../bin/acdk_core_r.lib "" /nologo /machine:I386 /out:"../../bin/acdk_doc_urlify_r.exe"

!ELSEIF  "$(CFG)" == "acdk_doc_urlify - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "../../bin"
# PROP BASE Intermediate_Dir "../obj/acdk_doc_urlify/dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../bin"
# PROP Intermediate_Dir "../obj/acdk_doc_urlify/dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od /I "../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /FR /Zm400 /c
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ../../bin/acdk_core_d.lib "" /nologo /debug /machine:I386 /out:"../../bin/acdk_doc_urlify_d.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "acdk_doc_urlify - Win32 Release"
# Name "acdk_doc_urlify - Win32 Debug"
# Begin Group "acdk"

# PROP Default_Filter ""
# Begin Group "doc"

# PROP Default_Filter ""
# Begin Group "urlify"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/doc/urlify/HTMLStreamTokenizer.cpp
DEP_CPP_HTMLS=\
	"..\..\include\acdk.h"\
	"..\..\include\acdk\acdk.h"\
	"..\..\include\acdk\Compiler.h"\
	"..\..\include\acdk\Config.h"\
	"..\..\include\acdk\io\AbstractCharFilterReader.h"\
	"..\..\include\acdk\io\AbstractCharFilterWriter.h"\
	"..\..\include\acdk\io\AbstractCharReader.h"\
	"..\..\include\acdk\io\AbstractCharWriter.h"\
	"..\..\include\acdk\io\AbstractFilterReader.h"\
	"..\..\include\acdk\io\AbstractReader.h"\
	"..\..\include\acdk\io\CharReader.h"\
	"..\..\include\acdk\io\CharWriter.h"\
	"..\..\include\acdk\io\DataReader.h"\
	"..\..\include\acdk\io\DataWriter.h"\
	"..\..\include\acdk\io\FilterReader.h"\
	"..\..\include\acdk\io\FilterWriter.h"\
	"..\..\include\acdk\io\io.h"\
	"..\..\include\acdk\io\IOException.h"\
	"..\..\include\acdk\io\LineNumberCharReader.h"\
	"..\..\include\acdk\io\ObjectReader.h"\
	"..\..\include\acdk\io\ObjectWriter.h"\
	"..\..\include\acdk\io\PrintWriter.h"\
	"..\..\include\acdk\io\PushbackCharReader.h"\
	"..\..\include\acdk\io\Reader.h"\
	"..\..\include\acdk\io\Serializable.h"\
	"..\..\include\acdk\io\StreamTokenizer.h"\
	"..\..\include\acdk\io\Writer.h"\
	"..\..\include\acdk\lang\BasicArray.h"\
	"..\..\include\acdk\lang\Character.h"\
	"..\..\include\acdk\lang\Class.h"\
	"..\..\include\acdk\lang\ClassCastException.h"\
	"..\..\include\acdk\lang\ClassNotFoundException.h"\
	"..\..\include\acdk\lang\Cloneable.h"\
	"..\..\include\acdk\lang\Comparable.h"\
	"..\..\include\acdk\lang\dmi\AcdkDmiClient.h"\
	"..\..\include\acdk\lang\dmi\ClazzInfo.h"\
	"..\..\include\acdk\lang\dmi\DmiClient.h"\
	"..\..\include\acdk\lang\dmi\DmiObject.h"\
	"..\..\include\acdk\lang\dmi\DmiObjectArray.h"\
	"..\..\include\acdk\lang\dmi\DmiProxy.h"\
	"..\..\include\acdk\lang\dmi\MetaAttribute.h"\
	"..\..\include\acdk\lang\dmi\MetaInfo.h"\
	"..\..\include\acdk\lang\dmi\MetaInfoFlags.h"\
	"..\..\include\acdk\lang\dmi\MetaObject.h"\
	"..\..\include\acdk\lang\dmi\NamedArgs.h"\
	"..\..\include\acdk\lang\dmi\ScriptVar.h"\
	"..\..\include\acdk\lang\dmi\StdDispatch.h"\
	"..\..\include\acdk\lang\dmi\SysFields.h"\
	"..\..\include\acdk\lang\DmiException.h"\
	"..\..\include\acdk\lang\Exception.h"\
	"..\..\include\acdk\lang\IllegalAccessException.h"\
	"..\..\include\acdk\lang\IllegalArgumentException.h"\
	"..\..\include\acdk\lang\IndexOutOfBoundsException.h"\
	"..\..\include\acdk\lang\InOutPreDeclaration.h"\
	"..\..\include\acdk\lang\InstantiationException.h"\
	"..\..\include\acdk\lang\InterfaceBase.h"\
	"..\..\include\acdk\lang\InterruptedException.h"\
	"..\..\include\acdk\lang\lang.h"\
	"..\..\include\acdk\lang\NoSuchDmiElementException.h"\
	"..\..\include\acdk\lang\NoSuchFieldException.h"\
	"..\..\include\acdk\lang\NoSuchMethodException.h"\
	"..\..\include\acdk\lang\NumberFormatException.h"\
	"..\..\include\acdk\lang\Object.h"\
	"..\..\include\acdk\lang\ObjectArrayBase.h"\
	"..\..\include\acdk\lang\ObjectArrayImpl.h"\
	"..\..\include\acdk\lang\ObjectBase.h"\
	"..\..\include\acdk\lang\ObjectImpl.h"\
	"..\..\include\acdk\lang\ObjectInline.h"\
	"..\..\include\acdk\lang\Package.h"\
	"..\..\include\acdk\lang\ref\NotifyObjectEvent.h"\
	"..\..\include\acdk\lang\ref\PhantomReference.h"\
	"..\..\include\acdk\lang\ref\ref.h"\
	"..\..\include\acdk\lang\ref\Reference.h"\
	"..\..\include\acdk\lang\ref\ReferenceQueue.h"\
	"..\..\include\acdk\lang\ref\SoftReference.h"\
	"..\..\include\acdk\lang\ref\WeakReference.h"\
	"..\..\include\acdk\lang\reflect\AccessibleObject.h"\
	"..\..\include\acdk\lang\reflect\Constructor.h"\
	"..\..\include\acdk\lang\reflect\Field.h"\
	"..\..\include\acdk\lang\reflect\InvocationTargetException.h"\
	"..\..\include\acdk\lang\reflect\Member.h"\
	"..\..\include\acdk\lang\reflect\Method.h"\
	"..\..\include\acdk\lang\reflect\Modifier.h"\
	"..\..\include\acdk\lang\reflect\Parameter.h"\
	"..\..\include\acdk\lang\reflect\reflect.h"\
	"..\..\include\acdk\lang\RuntimeException.h"\
	"..\..\include\acdk\lang\StackTraceElement.h"\
	"..\..\include\acdk\lang\String.h"\
	"..\..\include\acdk\lang\StringBuffer.h"\
	"..\..\include\acdk\lang\StringConcenator.h"\
	"..\..\include\acdk\lang\StringInline.h"\
	"..\..\include\acdk\lang\StringUtf8Utils.h"\
	"..\..\include\acdk\lang\sys\Allocator.h"\
	"..\..\include\acdk\lang\sys\BackTrace.h"\
	"..\..\include\acdk\lang\sys\ControlPushPopOnStack.h"\
	"..\..\include\acdk\lang\sys\core_alloca.h"\
	"..\..\include\acdk\lang\sys\core_atomicop.h"\
	"..\..\include\acdk\lang\sys\core_atomicop_linux.h"\
	"..\..\include\acdk\lang\sys\core_condition.h"\
	"..\..\include\acdk\lang\sys\core_fastmutex.h"\
	"..\..\include\acdk\lang\sys\core_guard.h"\
	"..\..\include\acdk\lang\sys\core_mutex.h"\
	"..\..\include\acdk\lang\sys\core_smartptr.h"\
	"..\..\include\acdk\lang\sys\core_static_vector.h"\
	"..\..\include\acdk\lang\sys\core_string.h"\
	"..\..\include\acdk\lang\sys\core_system.h"\
	"..\..\include\acdk\lang\sys\core_threadsys.h"\
	"..\..\include\acdk\lang\sys\core_vector.h"\
	"..\..\include\acdk\lang\sys\HeapFrame.h"\
	"..\..\include\acdk\lang\sys\ObjectHeap.h"\
	"..\..\include\acdk\lang\sys\ObjectLockPool.h"\
	"..\..\include\acdk\lang\sys\RefHeap.h"\
	"..\..\include\acdk\lang\sys\RefHolder.h"\
	"..\..\include\acdk\lang\sys\RefHolder3.h"\
	"..\..\include\acdk\lang\sys\RefHolderExt.h"\
	"..\..\include\acdk\lang\sys\StaticObjectWrapper.h"\
	"..\..\include\acdk\lang\sys\sys.h"\
	"..\..\include\acdk\lang\sys\SysRefHolder.h"\
	"..\..\include\acdk\lang\sys\TraceCallStack.h"\
	"..\..\include\acdk\lang\ThreadImpl.h"\
	"..\..\include\acdk\lang\Throwable.h"\
	"..\..\include\acdk\lang\UnicodeCharacter.h"\
	"..\..\include\acdk\lang\UnsupportedOperationException.h"\
	"..\..\include\acdk\locale\CodingErrorAction.h"\
	"..\..\include\acdk\locale\Decoder.h"\
	"..\..\include\acdk\locale\Encoder.h"\
	"..\..\include\acdk\Platform.h"\
	"..\..\include\acdk\util\util.h"\
	".\acdk\doc\urlify\HTMLStreamTokenizer.h"\
	
NODEP_CPP_HTMLS=\
	"..\..\include\acdk\lang\sys\Fields.h"\
	"..\..\include\acdk\lang\sys\ScriptVar.h"\
	
# End Source File
# Begin Source File

SOURCE=acdk/doc/urlify/HTMLStreamTokenizer.h
# End Source File
# Begin Source File

SOURCE=acdk/doc/urlify/urlifiy_acdk_doc.cpp
DEP_CPP_URLIF=\
	"..\..\include\acdk.h"\
	"..\..\include\acdk\acdk.h"\
	"..\..\include\acdk\Compiler.h"\
	"..\..\include\acdk\Config.h"\
	"..\..\include\acdk\io\AbstractCharFilterReader.h"\
	"..\..\include\acdk\io\AbstractCharFilterWriter.h"\
	"..\..\include\acdk\io\AbstractCharReader.h"\
	"..\..\include\acdk\io\AbstractCharWriter.h"\
	"..\..\include\acdk\io\AbstractFilterReader.h"\
	"..\..\include\acdk\io\AbstractFilterWriter.h"\
	"..\..\include\acdk\io\AbstractReader.h"\
	"..\..\include\acdk\io\AbstractStorageReader.h"\
	"..\..\include\acdk\io\AbstractStorageWriter.h"\
	"..\..\include\acdk\io\AbstractWriter.h"\
	"..\..\include\acdk\io\BufferedWriter.h"\
	"..\..\include\acdk\io\ByteToCharReader.h"\
	"..\..\include\acdk\io\CharReader.h"\
	"..\..\include\acdk\io\CharToByteWriter.h"\
	"..\..\include\acdk\io\CharWriter.h"\
	"..\..\include\acdk\io\DataReader.h"\
	"..\..\include\acdk\io\DataWriter.h"\
	"..\..\include\acdk\io\File.h"\
	"..\..\include\acdk\io\FileDescriptor.h"\
	"..\..\include\acdk\io\FileFilter.h"\
	"..\..\include\acdk\io\FileImpl.h"\
	"..\..\include\acdk\io\FilenameFilter.h"\
	"..\..\include\acdk\io\FileReader.h"\
	"..\..\include\acdk\io\FileReaderWriterImpl.h"\
	"..\..\include\acdk\io\FileSystem.h"\
	"..\..\include\acdk\io\FileSystemFactory.h"\
	"..\..\include\acdk\io\FileWriter.h"\
	"..\..\include\acdk\io\FilterReader.h"\
	"..\..\include\acdk\io\FilterWriter.h"\
	"..\..\include\acdk\io\InputReader.h"\
	"..\..\include\acdk\io\io.h"\
	"..\..\include\acdk\io\IOException.h"\
	"..\..\include\acdk\io\LineNumberCharReader.h"\
	"..\..\include\acdk\io\ObjectReader.h"\
	"..\..\include\acdk\io\ObjectWriter.h"\
	"..\..\include\acdk\io\PrintWriter.h"\
	"..\..\include\acdk\io\PushbackCharReader.h"\
	"..\..\include\acdk\io\Reader.h"\
	"..\..\include\acdk\io\Serializable.h"\
	"..\..\include\acdk\io\StandardFileSystem.h"\
	"..\..\include\acdk\io\StreamTokenizer.h"\
	"..\..\include\acdk\io\Writer.h"\
	"..\..\include\acdk\lang\ArrayIndexOutOfBoundsException.h"\
	"..\..\include\acdk\lang\BasicArray.h"\
	"..\..\include\acdk\lang\Boolean.h"\
	"..\..\include\acdk\lang\Class.h"\
	"..\..\include\acdk\lang\ClassCastException.h"\
	"..\..\include\acdk\lang\ClassNotFoundException.h"\
	"..\..\include\acdk\lang\Cloneable.h"\
	"..\..\include\acdk\lang\Comparable.h"\
	"..\..\include\acdk\lang\dmi\AcdkDmiClient.h"\
	"..\..\include\acdk\lang\dmi\ClazzInfo.h"\
	"..\..\include\acdk\lang\dmi\DmiClient.h"\
	"..\..\include\acdk\lang\dmi\DmiObject.h"\
	"..\..\include\acdk\lang\dmi\DmiObjectArray.h"\
	"..\..\include\acdk\lang\dmi\DmiProxy.h"\
	"..\..\include\acdk\lang\dmi\MetaAttribute.h"\
	"..\..\include\acdk\lang\dmi\MetaInfo.h"\
	"..\..\include\acdk\lang\dmi\MetaInfoFlags.h"\
	"..\..\include\acdk\lang\dmi\MetaObject.h"\
	"..\..\include\acdk\lang\dmi\NamedArgs.h"\
	"..\..\include\acdk\lang\dmi\ScriptVar.h"\
	"..\..\include\acdk\lang\dmi\StdDispatch.h"\
	"..\..\include\acdk\lang\dmi\SysFields.h"\
	"..\..\include\acdk\lang\DmiException.h"\
	"..\..\include\acdk\lang\Exception.h"\
	"..\..\include\acdk\lang\IllegalAccessException.h"\
	"..\..\include\acdk\lang\IllegalArgumentException.h"\
	"..\..\include\acdk\lang\IndexOutOfBoundsException.h"\
	"..\..\include\acdk\lang\InOutPreDeclaration.h"\
	"..\..\include\acdk\lang\InstantiationException.h"\
	"..\..\include\acdk\lang\InterfaceBase.h"\
	"..\..\include\acdk\lang\InterruptedException.h"\
	"..\..\include\acdk\lang\lang.h"\
	"..\..\include\acdk\lang\Math.h"\
	"..\..\include\acdk\lang\NoSuchDmiElementException.h"\
	"..\..\include\acdk\lang\NoSuchFieldException.h"\
	"..\..\include\acdk\lang\NoSuchMethodException.h"\
	"..\..\include\acdk\lang\NullPointerException.h"\
	"..\..\include\acdk\lang\NumberFormatException.h"\
	"..\..\include\acdk\lang\Object.h"\
	"..\..\include\acdk\lang\ObjectArrayBase.h"\
	"..\..\include\acdk\lang\ObjectArrayImpl.h"\
	"..\..\include\acdk\lang\ObjectBase.h"\
	"..\..\include\acdk\lang\ObjectImpl.h"\
	"..\..\include\acdk\lang\ObjectInline.h"\
	"..\..\include\acdk\lang\Package.h"\
	"..\..\include\acdk\lang\ref\NotifyObjectEvent.h"\
	"..\..\include\acdk\lang\ref\PhantomReference.h"\
	"..\..\include\acdk\lang\ref\ref.h"\
	"..\..\include\acdk\lang\ref\Reference.h"\
	"..\..\include\acdk\lang\ref\ReferenceQueue.h"\
	"..\..\include\acdk\lang\ref\SoftReference.h"\
	"..\..\include\acdk\lang\ref\WeakReference.h"\
	"..\..\include\acdk\lang\reflect\AccessibleObject.h"\
	"..\..\include\acdk\lang\reflect\Constructor.h"\
	"..\..\include\acdk\lang\reflect\Field.h"\
	"..\..\include\acdk\lang\reflect\InvocationTargetException.h"\
	"..\..\include\acdk\lang\reflect\Member.h"\
	"..\..\include\acdk\lang\reflect\Method.h"\
	"..\..\include\acdk\lang\reflect\Modifier.h"\
	"..\..\include\acdk\lang\reflect\Parameter.h"\
	"..\..\include\acdk\lang\reflect\reflect.h"\
	"..\..\include\acdk\lang\Runnable.h"\
	"..\..\include\acdk\lang\RuntimeException.h"\
	"..\..\include\acdk\lang\StackTraceElement.h"\
	"..\..\include\acdk\lang\String.h"\
	"..\..\include\acdk\lang\StringBuffer.h"\
	"..\..\include\acdk\lang\StringConcenator.h"\
	"..\..\include\acdk\lang\StringInline.h"\
	"..\..\include\acdk\lang\StringUtf8Utils.h"\
	"..\..\include\acdk\lang\sys\Allocator.h"\
	"..\..\include\acdk\lang\sys\BackTrace.h"\
	"..\..\include\acdk\lang\sys\ControlPushPopOnStack.h"\
	"..\..\include\acdk\lang\sys\core_alloca.h"\
	"..\..\include\acdk\lang\sys\core_atomicop.h"\
	"..\..\include\acdk\lang\sys\core_atomicop_linux.h"\
	"..\..\include\acdk\lang\sys\core_condition.h"\
	"..\..\include\acdk\lang\sys\core_fastmutex.h"\
	"..\..\include\acdk\lang\sys\core_guard.h"\
	"..\..\include\acdk\lang\sys\core_mutex.h"\
	"..\..\include\acdk\lang\sys\core_semaphore.h"\
	"..\..\include\acdk\lang\sys\core_smartptr.h"\
	"..\..\include\acdk\lang\sys\core_static_vector.h"\
	"..\..\include\acdk\lang\sys\core_string.h"\
	"..\..\include\acdk\lang\sys\core_system.h"\
	"..\..\include\acdk\lang\sys\core_threadsys.h"\
	"..\..\include\acdk\lang\sys\core_vector.h"\
	"..\..\include\acdk\lang\sys\HeapFrame.h"\
	"..\..\include\acdk\lang\sys\ObjectHeap.h"\
	"..\..\include\acdk\lang\sys\ObjectLockPool.h"\
	"..\..\include\acdk\lang\sys\RefHeap.h"\
	"..\..\include\acdk\lang\sys\RefHolder.h"\
	"..\..\include\acdk\lang\sys\RefHolder3.h"\
	"..\..\include\acdk\lang\sys\RefHolderExt.h"\
	"..\..\include\acdk\lang\sys\StaticObjectWrapper.h"\
	"..\..\include\acdk\lang\sys\sys.h"\
	"..\..\include\acdk\lang\sys\SysRefHolder.h"\
	"..\..\include\acdk\lang\sys\TraceCallStack.h"\
	"..\..\include\acdk\lang\System.h"\
	"..\..\include\acdk\lang\SystemError.h"\
	"..\..\include\acdk\lang\Thread.h"\
	"..\..\include\acdk\lang\ThreadGroup.h"\
	"..\..\include\acdk\lang\ThreadImpl.h"\
	"..\..\include\acdk\lang\ThreadLocalImpl.h"\
	"..\..\include\acdk\lang\Throwable.h"\
	"..\..\include\acdk\lang\UnsupportedOperationException.h"\
	"..\..\include\acdk\locale\ByteAsciiEncoding.h"\
	"..\..\include\acdk\locale\CodingErrorAction.h"\
	"..\..\include\acdk\locale\Decoder.h"\
	"..\..\include\acdk\locale\Encoder.h"\
	"..\..\include\acdk\locale\Encoding.h"\
	"..\..\include\acdk\locale\IllegalCharsetNameException.h"\
	"..\..\include\acdk\Platform.h"\
	"..\..\include\acdk\util\AbstractCollection.h"\
	"..\..\include\acdk\util\AbstractList.h"\
	"..\..\include\acdk\util\AbstractMap.h"\
	"..\..\include\acdk\util\AbstractSet.h"\
	"..\..\include\acdk\util\Arrays.h"\
	"..\..\include\acdk\util\BasicMapEntry.h"\
	"..\..\include\acdk\util\Bucket.h"\
	"..\..\include\acdk\util\Collection.h"\
	"..\..\include\acdk\util\Comparator.h"\
	"..\..\include\acdk\util\ConcurrentModificationException.h"\
	"..\..\include\acdk\util\HashMap.h"\
	"..\..\include\acdk\util\HashSet.h"\
	"..\..\include\acdk\util\Iterator.h"\
	"..\..\include\acdk\util\Map.h"\
	"..\..\include\acdk\util\NoSuchElementException.h"\
	"..\..\include\acdk\util\Properties.h"\
	"..\..\include\acdk\util\Set.h"\
	"..\..\include\acdk\util\util.h"\
	"..\..\include\acdk\util\Vector.h"\
	".\acdk\doc\urlify\HTMLStreamTokenizer.h"\
	
NODEP_CPP_URLIF=\
	"..\..\include\acdk\lang\sys\Fields.h"\
	"..\..\include\acdk\lang\sys\ScriptVar.h"\
	
# End Source File
# End Group
# End Group
# End Group
# End Target
# End Project
