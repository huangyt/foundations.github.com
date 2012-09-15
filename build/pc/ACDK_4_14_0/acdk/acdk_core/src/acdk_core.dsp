# Microsoft Developer Studio Project File - Name="acdk_core" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_core - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_core.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_core.mak" CFG="acdk_core - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_core - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE "acdk_core - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_core - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_core\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_core\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /EHsc /c
# ADD CPP /nologo /MD /GR /O2 /I "../src" /I "../include" /I "../../include" /D "OS_WIN32" /D "IN_ACDK_CORE_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /YX /FD /EHsc /Zm400 /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib "" /nologo /dll /debug /debugtype:coff /machine:I386 /out:"..\..\bin\acdk_core_r.dll"

!ELSEIF  "$(CFG)" == "acdk_core - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_core\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_core\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /EHsc /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /Zi /Od /I "../src" /I "../include" /I "../../include" /D "OS_WIN32" /D "IN_ACDK_CORE_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /FD /EHsc /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib "" /nologo /dll /incremental:no /debug /machine:I386 /out:"..\..\bin\acdk_core_d.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "acdk_core - Win32 Release"
# Name "acdk_core - Win32 Debug"
# Begin Group "acdk"

# PROP Default_Filter ""
# Begin Group "lang"

# PROP Default_Filter ""
# Begin Group "dmi"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/lang/dmi\AcdkDmiClient.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\AcdkDmiClient.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\AcdkStdWeakTypeDmiClient.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\AcdkStdWeakTypeDmiClient.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ClazzAttributesRes.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ClazzInfo.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ClazzInfo.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ClazzInfoInternals.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\dmi.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\dmi_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiClient.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiClient.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiDelegate.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiDelegate.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiNamedArg.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiObject.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiObject.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiObjectArray.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiProxy.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\DmiProxy.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\Marshaler.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\Marshaler.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaAttribute.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaAttribute.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaInfo.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaInfo.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaInfoChildsArray.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaInfoFlags.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaObject.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\MetaObject.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\NamedArgs.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ScriptInterpreter.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ScriptVar.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ScriptVar.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ScriptVar2.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\ScriptVarInl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\StdDispatch.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\StdDispatch.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\SysFields.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/dmi\SysFields.h
# End Source File
# End Group
# Begin Group "sys"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/lang/sys\Allocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\Allocator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\AllocatorInternals.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BackTrace.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BackTrace.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BitmapPagedAllocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BitmapPagedAllocator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BoehmGC.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BoehmGC.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BoehmGCAllocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\BoehmGCAllocator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\ControlPushPopOnStack.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_alloca.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_atomicop.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_atomicop_linux.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_condition.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_condition.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_fastmutex.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_fastmutex.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_guard.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_hashmap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_memcheck.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_memcheck.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_memtrace.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_memtrace.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_mutex.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_mutex.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_pair.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_prim.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_ptherror.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_recursivemutex.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_recursivemutex.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_semaphore.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_semaphore.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_sharedlib.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_smartptr.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_specific.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_specific.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_static_vector.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_string.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_substring.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_sys_static_mutex.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_syslocal.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_system.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_system.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_thread_id.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_threadsys.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_tick.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_value_scope.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_vector.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_winimage.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\core_winimage.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\default_vc_warnings.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\disable_vc_warnings.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\Garbage_Heap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\Garbage_Heap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\HeapFrame.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\LocalGcHeap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\LocalGcHeap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\MarkSweepGc.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\ObjectHeap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\ObjectHeap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\ObjectLockPool.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\ObjectLockPool.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\PagedAllocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\PagedAllocator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\PagedHeap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\PagedHeap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RC_GC_Heap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RC_GC_Heap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RC_Heap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RC_Heap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHeap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder1.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder2.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder3.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder3Inl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder4.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolder5.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolderExt.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\RefHolderX.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\StackAllocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\StackAllocator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\StackHeap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\StackHeap.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\StaticObjectWrapper.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\sys.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\SysRefHolder.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\TracedRawAllocator.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/sys\TracedRawAllocator.h
# End Source File
# End Group
# Begin Group "ref"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/lang/ref\acdk_lang_ref_Package.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\NotifyObjectEvent.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\NotifyObjectEvent.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\PhantomReference.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\ref.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\ref_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\Reference.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\Reference.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\ReferenceQueue.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\ReferenceQueue.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\SharedOwning.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\SharedOwning.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\SoftReference.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/ref\WeakReference.h
# End Source File
# End Group
# Begin Group "reflect"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/lang/reflect\AccessibleObject.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Constructor.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Constructor.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Enumeration.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Enumeration.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Field.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Field.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\InvocationTargetException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Member.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Method.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Method.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Modifier.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Modifier.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Parameter.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\reflect.h
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\reflect_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Unit.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang/reflect\Unit.h
# End Source File
# End Group
# Begin Source File

SOURCE=acdk/lang\acdk_lang_Package.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ArithmeticException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ArrayIndexOutOfBoundsException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\BasicArray.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\BasicArray.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\BasicArrayInl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Boolean.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Boolean.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Byte.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Byte.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ByteBuffer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ByteBuffer.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Character.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Character.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Class.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Class.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ClassCastException.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ClassCastException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ClassLoader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ClassLoader.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ClassNotFoundException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Cloneable.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\CloneNotSupportedException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\CmdLineOption.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\CmdLineOption.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\CmdLineParseException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\CmdLineParser.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\CmdLineParser.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Comparable.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\DmiException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\DmiTypeConversionException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Double.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Double.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Error.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Exception.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ExceptionDeclarations.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ExtObject.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Float.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Float.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IllegalAccessException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IllegalArgumentException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IllegalMonitorStateException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IllegalStateException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IllegalThreadStateException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\IndexOutOfBoundsException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\InOutPreDeclaration.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\InstantiationException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Integer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Integer.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\InterfaceBase.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\InterruptedException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\lang.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\lang_all.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\lang_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Long.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Long.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Math.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Math.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NoSuchDmiElementException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NoSuchElementException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NoSuchFieldException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NoSuchMethodException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NullPointerException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Number.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Number.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\NumberFormatException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Object.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Object.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectArrayBase.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectArrayBase.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectArrayBaseInl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectArrayImpl.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectArrayImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectBase.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectDebug.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectDebug.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ObjectInline.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\OutOfMemoryError.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Package.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Package.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ParamsMismatchException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Process.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Process.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Runnable.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Runtime.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Runtime.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\RuntimeException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\SharedLibrary.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\SharedLibrary.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Short.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Short.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StackFrame.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\StackFrame.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StackOverflowError.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\String.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\String.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringBuffer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringConcenator.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringIndexOutOfBoundsException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringInline.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringInternals.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringOld.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringUtf8Utils.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\StringUtf8Utils.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\System.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\System.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\SystemError.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\SystemError.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\SystemInline.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\SystemIntern.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Thread.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Thread.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadDeath.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadGroup.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadGroup.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadLocal.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\ThreadLocalImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Throwable.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Throwable.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\UnicodeCharacter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\UnicodeCharacter.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\UnsupportedOperationException.h
# End Source File
# Begin Source File

SOURCE=acdk/lang\Void.cpp
# End Source File
# Begin Source File

SOURCE=acdk/lang\Void.h
# End Source File
# End Group
# Begin Group "io"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/io\AbstractCharFilterReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharFilterReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharFilterWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractCharWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractFilterReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractFilterWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractFilterWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractObjectReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractObjectReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractObjectWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractObjectWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractStorageReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractStorageWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\AbstractWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\acdk_io_Package.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ASCIIDataReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ASCIIDataReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ASCIIDataWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ASCIIDataWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryDataReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryDataReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryDataWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryDataWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryObjectReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryObjectReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryObjectWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BinaryObjectWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BufferedReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BufferedReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BufferedWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\BufferedWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteBufferReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteBufferReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteBufferWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteBufferWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\BytePtrReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteToCharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteToCharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ByteToCharWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharArrayReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\CharArrayReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharArrayWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\CharArrayWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\CharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharToByteReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharToByteWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\CharToByteWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\CharWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleCharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleCharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleCharWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleCharWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\ConsoleWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\DataReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\DataWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\EOFException.h
# End Source File
# Begin Source File

SOURCE=acdk/io\File.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\File.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileAbstractImpl.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileAbstractImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileDescriptor.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileDescriptor.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileFilter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileImpl.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileInfo.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileInfo.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FilenameFilter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileNotFoundException.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileReaderWriterImpl.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileReaderWriterImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileStandardImpl.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileStandardImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileStatus.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileStatus.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileSystem.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileSystem.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileSystemFactory.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FileWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\FileWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FilterReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\FilterWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\GlobFilenameFilter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\GlobFilenameFilter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\InputReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\InputReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\io.h
# End Source File
# Begin Source File

SOURCE=acdk/io\io_all.h
# End Source File
# Begin Source File

SOURCE=acdk/io\io_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\IOException.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\IOException.h
# End Source File
# Begin Source File

SOURCE=acdk/io\JoinedReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\JoinedReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\LineNumberCharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\LineNumberCharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\LineNumberReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\LineNumberReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\MemoryMappedFile.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\MemoryMappedFile.h
# End Source File
# Begin Source File

SOURCE=acdk/io\MemReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\MemReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\MemWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\MemWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\NullWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ObjectReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ObjectStreamException.h
# End Source File
# Begin Source File

SOURCE=acdk/io\ObjectWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\OutputDebugStringWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\PipedReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\PipedReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\PipedWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\PipedWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\PrintWriter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\PrintWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\PushbackCharReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\PushbackCharReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\PushbackReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\PushbackReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\RandomAccessFile.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\RandomAccessFile.h
# End Source File
# Begin Source File

SOURCE=acdk/io\Reader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\Reader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\RessourceFileImpl.h
# End Source File
# Begin Source File

SOURCE=acdk/io\RessourceFileSystem.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\RessourceFileSystem.h
# End Source File
# Begin Source File

SOURCE=acdk/io\Serializable.h
# End Source File
# Begin Source File

SOURCE=acdk/io\SerializedObjectDescriptor.h
# End Source File
# Begin Source File

SOURCE=acdk/io\StandardFileSystem.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\StandardFileSystem.h
# End Source File
# Begin Source File

SOURCE=acdk/io\Storage.h
# End Source File
# Begin Source File

SOURCE=acdk/io\StreamTokenizer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\StreamTokenizer.h
# End Source File
# Begin Source File

SOURCE=acdk/io\StringReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\StringWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\SubReader.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\SubReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\TeeCharWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\TeeReader.h
# End Source File
# Begin Source File

SOURCE=acdk/io\TeeWriter.h
# End Source File
# Begin Source File

SOURCE=acdk/io\Writer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/io\Writer.h
# End Source File
# End Group
# Begin Group "util"

# PROP Default_Filter ""
# Begin Group "logging"

# PROP Default_Filter ""
# Begin Source File

SOURCE=acdk/util/logging\AbstractLogConsumer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\AbstractLogConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\ConsoleConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\FileConsumer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\FileConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Formatter.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Level.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Level.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Log.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\LogConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Logger.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Logger.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\logging.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\logging_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\LogManager.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\LogManager.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\LogRecord.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\MsgBoxConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\NamedLogArgs.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\RollingFileConsumer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\RollingFileConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\SimpleFormatter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\SimpleFormatter.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\StdFormatter.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\StdFormatter.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\TransactionConsumer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\TransactionConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Win32DbgConsumer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\Win32DbgConsumer.h
# End Source File
# Begin Source File

SOURCE=acdk/util/logging\WriterConsumer.h
# End Source File
# End Group
# Begin Source File

SOURCE=acdk/util\AbstractCollection.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractCollection.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractList.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractListListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractListSubList.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractListSubList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractMap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractSequentialList.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractSequentialList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractSet.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\AbstractSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\acdk_util_classes.h
# End Source File
# Begin Source File

SOURCE=acdk/util\acdk_util_Package.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\ArrayIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\ArrayList.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\ArrayList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Arrays.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Arrays.h
# End Source File
# Begin Source File

SOURCE=acdk/util\BasicMapEntry.h
# End Source File
# Begin Source File

SOURCE=acdk/util\BitSet.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\BitSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Bucket.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Bucket.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Calendar.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Calendar.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Collection.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Collections.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Collections.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Comparator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\ConcurrentModificationException.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Date.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Date.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Dictionary.h
# End Source File
# Begin Source File

SOURCE=acdk/util\DoubleIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\EmptyCollectionIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Enumeration.h
# End Source File
# Begin Source File

SOURCE=acdk/util\EventListener.h
# End Source File
# Begin Source File

SOURCE=acdk/util\EventListenerProxy.h
# End Source File
# Begin Source File

SOURCE=acdk/util\EventObject.h
# End Source File
# Begin Source File

SOURCE=acdk/util\GregorianCalendar.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\GregorianCalendar.h
# End Source File
# Begin Source File

SOURCE=acdk/util\HashMap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\HashMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\HashSet.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\HashSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Hashtable.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Hashtable.h
# End Source File
# Begin Source File

SOURCE=acdk/util\IdentityHashMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Iterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\LinkedList.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\LinkedList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\List.h
# End Source File
# Begin Source File

SOURCE=acdk/util\ListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\ListResourceBundle.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\ListResourceBundle.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Locale.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Locale.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Map.h
# End Source File
# Begin Source File

SOURCE=acdk/util\MissingResourceException.h
# End Source File
# Begin Source File

SOURCE=acdk/util\NoSuchElementException.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Properties.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Properties.h
# End Source File
# Begin Source File

SOURCE=acdk/util\PropertiesListener.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\PropertiesListener.h
# End Source File
# Begin Source File

SOURCE=acdk/util\PropertyResourceBundle.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\PropertyResourceBundle.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Random.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Random.h
# End Source File
# Begin Source File

SOURCE=acdk/util\ResourceBundle.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\ResourceBundle.h
# End Source File
# Begin Source File

SOURCE=acdk/util\Set.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleCalendar.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleCalendar.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleListResourceBundle.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleListResourceBundle.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleTimeZone.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\SimpleTimeZone.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SingleObjectIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SortedMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SortedSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\StringTokenizer.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\StringTokenizer.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SynchronizedCollections.h
# End Source File
# Begin Source File

SOURCE=acdk/util\SysDate.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\SysDate.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractCollection.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractListListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractListSubList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TAbstractSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TArrayList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TBasicMapEntry.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TBucket.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TCollection.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TComparator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TDoubleIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\THashMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\THashSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TimeZone.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\TimeZone.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TList.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TListIterator.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TreeMap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\TreeMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TreeSet.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\TreeSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TSet.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TSortedMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\TTreeMap.h
# End Source File
# Begin Source File

SOURCE=acdk/util\util.h
# End Source File
# Begin Source File

SOURCE=acdk/util\util_all.h
# End Source File
# Begin Source File

SOURCE=acdk/util\util_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Vector.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\Vector.h
# End Source File
# Begin Source File

SOURCE=acdk/util\WeakHashMap.cpp
# End Source File
# Begin Source File

SOURCE=acdk/util\WeakHashMap.h
# End Source File
# End Group
# Begin Group "locale"

# PROP Default_Filter ""
# Begin Source File

SOURCE="acdk/locale\8859-10_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-13_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-14_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-15_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-16_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-1_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-2_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-3_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-4_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-5_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-6_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-7_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-8_mapping.h"
# End Source File
# Begin Source File

SOURCE="acdk/locale\8859-9_mapping.h"
# End Source File
# Begin Source File

SOURCE=acdk/locale\AsciiEncoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\AsciiEncoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\AsciiUtfEncoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\AsciiUtfEncoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\ByteAsciiEncoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\ByteAsciiEncoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CEscapeEncoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\CEscapeEncoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CharacterCodingException.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CodingErrorAction.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1250_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1251_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1252_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1253_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1254_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1255_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1256_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1257_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\CP1258_mapping.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\Decoder.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\Encoder.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\Encoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\Encoding.h
# End Source File
# Begin Source File

SOURCE="acdk/locale\IBM-850_mapping.h"
# End Source File
# Begin Source File

SOURCE=acdk/locale\IllegalCharsetNameException.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\IsoEncoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\IsoEncoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\IsoTables.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\locale.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\locale_metainf_base.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\UCS2Encoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\UCS2Encoding.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\UnicodeTable.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\UnicodeTable.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\UnmappableCharacterException.h
# End Source File
# Begin Source File

SOURCE=acdk/locale\UTF8Encoding.cpp
# End Source File
# Begin Source File

SOURCE=acdk/locale\UTF8Encoding.h
# End Source File
# End Group
# Begin Source File

SOURCE=acdk\acdk.h
# End Source File
# Begin Source File

SOURCE=acdk\acdk_all.h
# End Source File
# Begin Source File

SOURCE=acdk\AcdkCoreConfig.h
# End Source File
# Begin Source File

SOURCE=acdk\Compiler.h
# End Source File
# Begin Source File

SOURCE=acdk\Config.h
# End Source File
# Begin Source File

SOURCE=acdk\Platform.h
# End Source File
# Begin Source File

SOURCE=acdk\Version.h
# End Source File
# End Group
# End Target
# End Project
