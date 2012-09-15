# Microsoft Developer Studio Project File - Name="acdkx_orb_Server_Test" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=acdkx_orb_Server_Test - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_orb_Server_Test.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdkx_orb_Server_Test.mak" CFG="acdkx_orb_Server_Test - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdkx_orb_Server_Test - Win32 Release" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE "acdkx_orb_Server_Test - Win32 Debug" (basierend auf  "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdkx_orb_Server_Test - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\..\..\bin"
# PROP BASE Intermediate_Dir "obj\acdkx_orb_Server_Test\dsp_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\bin"
# PROP Intermediate_Dir "obj\acdkx_orb_Server_Test\dsp_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /Zm200 /c
# ADD CPP /nologo /MD /Gi /GR /GX /O2 /I "../../../src" /I "../../../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "USE_ACDK_NET_LIB" /D "USE_ACDKX_ORB_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /c
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\bin\acdk_core_dll_r.lib ..\..\..\..\bin\acdk_net_dll_r.lib ..\..\..\..\bin\acdkx_orb_dll_r.lib "" /nologo /machine:I386 /out:"..\..\..\..\bin\acdkx_orb_Server_Test_r.exe"

!ELSEIF  "$(CFG)" == "acdkx_orb_Server_Test - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\..\..\bin"
# PROP BASE Intermediate_Dir "obj\acdkx_orb_Server_Test\dsp_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\bin"
# PROP Intermediate_Dir "obj\acdkx_orb_Server_Test\dsp_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /Zm200 /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /GX /Zi /Od /I "../../../src" /I "../../../../include" /D "OS_WIN32" /D "ACDK_USE_ACDK_LIB" /D "USE_ACDK_NET_LIB" /D "USE_ACDKX_ORB_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_USE_GC" /D "ACDK_DEBUG" /FR /c
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\..\..\bin\acdk_core_dll_d.lib ..\..\..\..\bin\acdk_net_dll_d.lib ..\..\..\..\bin\acdkx_orb_dll_d.lib "" /nologo /debug /machine:I386 /out:"..\..\..\..\bin\acdkx_orb_Server_Test_d.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "acdkx_orb_Server_Test - Win32 Release"
# Name "acdkx_orb_Server_Test - Win32 Debug"
# Begin Source File

SOURCE=acdkx_orb_Server_Test.cpp
DEP_CPP_ACDKX=\
	"..\..\..\..\include\acdk.h"\
	"..\..\..\..\include\acdk\acdk.h"\
	"..\..\..\..\include\acdk\acdk_all.h"\
	"..\..\..\..\include\acdk\Compiler.h"\
	"..\..\..\..\include\acdk\Config.h"\
	"..\..\..\..\include\acdk\io\AbstractFilterReader.h"\
	"..\..\..\..\include\acdk\io\AbstractFilterWriter.h"\
	"..\..\..\..\include\acdk\io\AbstractReader.h"\
	"..\..\..\..\include\acdk\io\AbstractStorageReader.h"\
	"..\..\..\..\include\acdk\io\AbstractStorageWriter.h"\
	"..\..\..\..\include\acdk\io\AbstractWriter.h"\
	"..\..\..\..\include\acdk\io\ASCIIDataReader.h"\
	"..\..\..\..\include\acdk\io\ASCIIDataWriter.h"\
	"..\..\..\..\include\acdk\io\BinaryDataReader.h"\
	"..\..\..\..\include\acdk\io\BinaryDataWriter.h"\
	"..\..\..\..\include\acdk\io\BufferedReader.h"\
	"..\..\..\..\include\acdk\io\BufferedWriter.h"\
	"..\..\..\..\include\acdk\io\CharArrayReader.h"\
	"..\..\..\..\include\acdk\io\CharArrayWriter.h"\
	"..\..\..\..\include\acdk\io\ConsoleReader.h"\
	"..\..\..\..\include\acdk\io\ConsoleWriter.h"\
	"..\..\..\..\include\acdk\io\DataReader.h"\
	"..\..\..\..\include\acdk\io\DataWriter.h"\
	"..\..\..\..\include\acdk\io\EOFException.h"\
	"..\..\..\..\include\acdk\io\File.h"\
	"..\..\..\..\include\acdk\io\FileDescriptor.h"\
	"..\..\..\..\include\acdk\io\FileFilter.h"\
	"..\..\..\..\include\acdk\io\FilenameFilter.h"\
	"..\..\..\..\include\acdk\io\FileNotFoundException.h"\
	"..\..\..\..\include\acdk\io\FileReader.h"\
	"..\..\..\..\include\acdk\io\FileReaderWriterImpl.h"\
	"..\..\..\..\include\acdk\io\FileWriter.h"\
	"..\..\..\..\include\acdk\io\FilterReader.h"\
	"..\..\..\..\include\acdk\io\FilterWriter.h"\
	"..\..\..\..\include\acdk\io\GlobFilenameFilter.h"\
	"..\..\..\..\include\acdk\io\InputReader.h"\
	"..\..\..\..\include\acdk\io\io.h"\
	"..\..\..\..\include\acdk\io\io_all.h"\
	"..\..\..\..\include\acdk\io\IOException.h"\
	"..\..\..\..\include\acdk\io\LineNumberReader.h"\
	"..\..\..\..\include\acdk\io\MemReader.h"\
	"..\..\..\..\include\acdk\io\MemWriter.h"\
	"..\..\..\..\include\acdk\io\NullWriter.h"\
	"..\..\..\..\include\acdk\io\ObjectReader.h"\
	"..\..\..\..\include\acdk\io\ObjectWriter.h"\
	"..\..\..\..\include\acdk\io\PipedReader.h"\
	"..\..\..\..\include\acdk\io\PipedReaderWriterInline.h"\
	"..\..\..\..\include\acdk\io\PipedWriter.h"\
	"..\..\..\..\include\acdk\io\PrintWriter.h"\
	"..\..\..\..\include\acdk\io\PushbackReader.h"\
	"..\..\..\..\include\acdk\io\RandomAccessFile.h"\
	"..\..\..\..\include\acdk\io\Reader.h"\
	"..\..\..\..\include\acdk\io\Serializable.h"\
	"..\..\..\..\include\acdk\io\StreamTokenizer.h"\
	"..\..\..\..\include\acdk\io\Writer.h"\
	"..\..\..\..\include\acdk\lang\ArrayIndexOutOfBoundsException.h"\
	"..\..\..\..\include\acdk\lang\BadCastException.h"\
	"..\..\..\..\include\acdk\lang\Boolean.h"\
	"..\..\..\..\include\acdk\lang\Byte.h"\
	"..\..\..\..\include\acdk\lang\Character.h"\
	"..\..\..\..\include\acdk\lang\Class.h"\
	"..\..\..\..\include\acdk\lang\ClassLoader.h"\
	"..\..\..\..\include\acdk\lang\ClassNotFoundException.h"\
	"..\..\..\..\include\acdk\lang\ClazzInfo.h"\
	"..\..\..\..\include\acdk\lang\Cloneable.h"\
	"..\..\..\..\include\acdk\lang\CloneNotSupportedException.h"\
	"..\..\..\..\include\acdk\lang\Comparable.h"\
	"..\..\..\..\include\acdk\lang\Double.h"\
	"..\..\..\..\include\acdk\lang\Exception.h"\
	"..\..\..\..\include\acdk\lang\IllegalAccessException.h"\
	"..\..\..\..\include\acdk\lang\IllegalArgumentException.h"\
	"..\..\..\..\include\acdk\lang\IllegalMonitorStateException.h"\
	"..\..\..\..\include\acdk\lang\IllegalStateException.h"\
	"..\..\..\..\include\acdk\lang\IllegalThreadStateException.h"\
	"..\..\..\..\include\acdk\lang\IndexOutOfBoundsException.h"\
	"..\..\..\..\include\acdk\lang\InOutPreDeclaration.h"\
	"..\..\..\..\include\acdk\lang\InstantiationException.h"\
	"..\..\..\..\include\acdk\lang\Integer.h"\
	"..\..\..\..\include\acdk\lang\InterruptedException.h"\
	"..\..\..\..\include\acdk\lang\lang.h"\
	"..\..\..\..\include\acdk\lang\lang_all.h"\
	"..\..\..\..\include\acdk\lang\Logger.h"\
	"..\..\..\..\include\acdk\lang\Long.h"\
	"..\..\..\..\include\acdk\lang\Math.h"\
	"..\..\..\..\include\acdk\lang\NullPointerException.h"\
	"..\..\..\..\include\acdk\lang\Number.h"\
	"..\..\..\..\include\acdk\lang\NumberFormatException.h"\
	"..\..\..\..\include\acdk\lang\Object.h"\
	"..\..\..\..\include\acdk\lang\ObjectBase.h"\
	"..\..\..\..\include\acdk\lang\ObjectImpl.h"\
	"..\..\..\..\include\acdk\lang\ObjectInline.h"\
	"..\..\..\..\include\acdk\lang\Package.h"\
	"..\..\..\..\include\acdk\lang\Process.h"\
	"..\..\..\..\include\acdk\lang\ref\NotifyObjectEvent.h"\
	"..\..\..\..\include\acdk\lang\ref\PhantomReference.h"\
	"..\..\..\..\include\acdk\lang\ref\ref.h"\
	"..\..\..\..\include\acdk\lang\ref\Reference.h"\
	"..\..\..\..\include\acdk\lang\ref\ReferenceQueue.h"\
	"..\..\..\..\include\acdk\lang\ref\SoftReference.h"\
	"..\..\..\..\include\acdk\lang\ref\WeakReference.h"\
	"..\..\..\..\include\acdk\lang\reflect\AccessibleObject.h"\
	"..\..\..\..\include\acdk\lang\reflect\Constructor.h"\
	"..\..\..\..\include\acdk\lang\reflect\Field.h"\
	"..\..\..\..\include\acdk\lang\reflect\InvocationTargetException.h"\
	"..\..\..\..\include\acdk\lang\reflect\Member.h"\
	"..\..\..\..\include\acdk\lang\reflect\Method.h"\
	"..\..\..\..\include\acdk\lang\reflect\Modifier.h"\
	"..\..\..\..\include\acdk\lang\reflect\reflect.h"\
	"..\..\..\..\include\acdk\lang\Runnable.h"\
	"..\..\..\..\include\acdk\lang\Runtime.h"\
	"..\..\..\..\include\acdk\lang\RuntimeException.h"\
	"..\..\..\..\include\acdk\lang\SharedLibrary.h"\
	"..\..\..\..\include\acdk\lang\Short.h"\
	"..\..\..\..\include\acdk\lang\String.h"\
	"..\..\..\..\include\acdk\lang\StringBuffer.h"\
	"..\..\..\..\include\acdk\lang\StringIndexOutOfBoundsException.h"\
	"..\..\..\..\include\acdk\lang\StringInline.h"\
	"..\..\..\..\include\acdk\lang\sys\Allocator.h"\
	"..\..\..\..\include\acdk\lang\sys\BasicArray.h"\
	"..\..\..\..\include\acdk\lang\sys\ControlPushPopOnStack.h"\
	"..\..\..\..\include\acdk\lang\sys\core_condition.h"\
	"..\..\..\..\include\acdk\lang\sys\core_fastmutex.h"\
	"..\..\..\..\include\acdk\lang\sys\core_mutex.h"\
	"..\..\..\..\include\acdk\lang\sys\core_string.h"\
	"..\..\..\..\include\acdk\lang\sys\core_vector.h"\
	"..\..\..\..\include\acdk\lang\sys\default_vc_warnings.h"\
	"..\..\..\..\include\acdk\lang\sys\disable_vc_warnings.h"\
	"..\..\..\..\include\acdk\lang\sys\Fields.h"\
	"..\..\..\..\include\acdk\lang\sys\Garbage_Heap.h"\
	"..\..\..\..\include\acdk\lang\sys\HeapFrame.h"\
	"..\..\..\..\include\acdk\lang\sys\ObjectArrayImpl.h"\
	"..\..\..\..\include\acdk\lang\sys\ObjectHeap.h"\
	"..\..\..\..\include\acdk\lang\sys\ObjectLockPool.h"\
	"..\..\..\..\include\acdk\lang\sys\PagedAllocator.h"\
	"..\..\..\..\include\acdk\lang\sys\RC_GC_Heap.h"\
	"..\..\..\..\include\acdk\lang\sys\RC_Heap.h"\
	"..\..\..\..\include\acdk\lang\sys\RefHolder.h"\
	"..\..\..\..\include\acdk\lang\sys\ScriptVar.h"\
	"..\..\..\..\include\acdk\lang\sys\stdio.h"\
	"..\..\..\..\include\acdk\lang\sys\sys.h"\
	"..\..\..\..\include\acdk\lang\sys\SysRefHolder.h"\
	"..\..\..\..\include\acdk\lang\sys\TraceCallStack.h"\
	"..\..\..\..\include\acdk\lang\sys\TracedRawAllocator.h"\
	"..\..\..\..\include\acdk\lang\System.h"\
	"..\..\..\..\include\acdk\lang\SystemError.h"\
	"..\..\..\..\include\acdk\lang\SystemInline.h"\
	"..\..\..\..\include\acdk\lang\Thread.h"\
	"..\..\..\..\include\acdk\lang\ThreadGroup.h"\
	"..\..\..\..\include\acdk\lang\ThreadID.h"\
	"..\..\..\..\include\acdk\lang\ThreadImpl.h"\
	"..\..\..\..\include\acdk\lang\ThreadLocal.h"\
	"..\..\..\..\include\acdk\lang\ThreadLocalImpl.h"\
	"..\..\..\..\include\acdk\lang\Throwable.h"\
	"..\..\..\..\include\acdk\lang\UnsupportedOperationException.h"\
	"..\..\..\..\include\acdk\lang\Void.h"\
	"..\..\..\..\include\acdk\net\Authenticator.h"\
	"..\..\..\..\include\acdk\net\Config.h"\
	"..\..\..\..\include\acdk\net\ContentHandler.h"\
	"..\..\..\..\include\acdk\net\ContentHandlerFactory.h"\
	"..\..\..\..\include\acdk\net\FileNameMap.h"\
	"..\..\..\..\include\acdk\net\HttpURLConnection.h"\
	"..\..\..\..\include\acdk\net\InetAddress.h"\
	"..\..\..\..\include\acdk\net\MalformedURLException.h"\
	"..\..\..\..\include\acdk\net\MimeTypeMapper.h"\
	"..\..\..\..\include\acdk\net\net.h"\
	"..\..\..\..\include\acdk\net\PasswordAuthentication.h"\
	"..\..\..\..\include\acdk\net\ProtocolException.h"\
	"..\..\..\..\include\acdk\net\ServerSocket.h"\
	"..\..\..\..\include\acdk\net\Socket.h"\
	"..\..\..\..\include\acdk\net\SocketException.h"\
	"..\..\..\..\include\acdk\net\SocketImpl.h"\
	"..\..\..\..\include\acdk\net\SocketImplFactory.h"\
	"..\..\..\..\include\acdk\net\SocketOptions.h"\
	"..\..\..\..\include\acdk\net\TCPSocket.h"\
	"..\..\..\..\include\acdk\net\TCPSocketFactory.h"\
	"..\..\..\..\include\acdk\net\UnknownServiceException.h"\
	"..\..\..\..\include\acdk\net\URL.h"\
	"..\..\..\..\include\acdk\net\URLConnection.h"\
	"..\..\..\..\include\acdk\net\URLDecoder.h"\
	"..\..\..\..\include\acdk\net\URLEncoder.h"\
	"..\..\..\..\include\acdk\net\URLInterface.h"\
	"..\..\..\..\include\acdk\net\URLStreamHandler.h"\
	"..\..\..\..\include\acdk\net\URLStreamHandlerFactory.h"\
	"..\..\..\..\include\acdk\Platform.h"\
	"..\..\..\..\include\acdk\util\AbstractCollection.h"\
	"..\..\..\..\include\acdk\util\AbstractList.h"\
	"..\..\..\..\include\acdk\util\AbstractListIterator.h"\
	"..\..\..\..\include\acdk\util\AbstractListListIterator.h"\
	"..\..\..\..\include\acdk\util\AbstractListSubList.h"\
	"..\..\..\..\include\acdk\util\AbstractMap.h"\
	"..\..\..\..\include\acdk\util\AbstractSequentialList.h"\
	"..\..\..\..\include\acdk\util\AbstractSet.h"\
	"..\..\..\..\include\acdk\util\acdk_util_classes.h"\
	"..\..\..\..\include\acdk\util\ArrayList.h"\
	"..\..\..\..\include\acdk\util\Arrays.h"\
	"..\..\..\..\include\acdk\util\BasicMapEntry.h"\
	"..\..\..\..\include\acdk\util\BitSet.h"\
	"..\..\..\..\include\acdk\util\Bucket.h"\
	"..\..\..\..\include\acdk\util\Calendar.h"\
	"..\..\..\..\include\acdk\util\Collection.h"\
	"..\..\..\..\include\acdk\util\Collections.h"\
	"..\..\..\..\include\acdk\util\Comparator.h"\
	"..\..\..\..\include\acdk\util\ConcurrentModificationException.h"\
	"..\..\..\..\include\acdk\util\Date.h"\
	"..\..\..\..\include\acdk\util\Dictionary.h"\
	"..\..\..\..\include\acdk\util\DoubleIterator.h"\
	"..\..\..\..\include\acdk\util\Enumeration.h"\
	"..\..\..\..\include\acdk\util\GregorianCalendar.h"\
	"..\..\..\..\include\acdk\util\HashMap.h"\
	"..\..\..\..\include\acdk\util\HashSet.h"\
	"..\..\..\..\include\acdk\util\Hashtable.h"\
	"..\..\..\..\include\acdk\util\Iterator.h"\
	"..\..\..\..\include\acdk\util\LinkedList.h"\
	"..\..\..\..\include\acdk\util\ListIterator.h"\
	"..\..\..\..\include\acdk\util\ListResourceBundle.h"\
	"..\..\..\..\include\acdk\util\Map.h"\
	"..\..\..\..\include\acdk\util\MissingResourceException.h"\
	"..\..\..\..\include\acdk\util\NoSuchElementException.h"\
	"..\..\..\..\include\acdk\util\Properties.h"\
	"..\..\..\..\include\acdk\util\PropertyResourceBundle.h"\
	"..\..\..\..\include\acdk\util\Random.h"\
	"..\..\..\..\include\acdk\util\ResourceBundle.h"\
	"..\..\..\..\include\acdk\util\Set.h"\
	"..\..\..\..\include\acdk\util\SimpleListResourceBundle.h"\
	"..\..\..\..\include\acdk\util\SimpleTimeZone.h"\
	"..\..\..\..\include\acdk\util\SortedMap.h"\
	"..\..\..\..\include\acdk\util\SortedSet.h"\
	"..\..\..\..\include\acdk\util\StringTokenizer.h"\
	"..\..\..\..\include\acdk\util\SynchronizedCollections.h"\
	"..\..\..\..\include\acdk\util\TimeZone.h"\
	"..\..\..\..\include\acdk\util\TreeMap.h"\
	"..\..\..\..\include\acdk\util\TreeSet.h"\
	"..\..\..\..\include\acdk\util\util.h"\
	"..\..\..\..\include\acdk\util\util_all.h"\
	"..\..\..\..\include\acdk\util\Vector.h"\
	"..\..\..\..\include\acdk\util\VectorInline.h"\
	"..\..\..\..\include\acdk_all.h"\
	"..\..\..\src\acdkx\arb\ADelegate.h"\
	"..\..\..\src\acdkx\arb\AObjectImpl.h"\
	"..\..\..\src\acdkx\arb\arb.h"\
	"..\..\..\src\acdkx\arb\Config.h"\
	"..\..\..\src\acdkx\orb\AORB.h"\
	"..\..\..\src\acdkx\orb\Config.h"\
	"..\..\..\src\acdkx\orb\orb.h"\
	"..\..\..\src\acdkx\orb\std_orb.h"\
	"..\..\..\src\org\omg\CORBA\CORBA.h"\
	"..\..\..\src\org\omg\CORBA\Object.h"\
	"..\..\..\src\org\omg\CORBA\ORB.h"\
	"..\..\..\src\org\omg\CORBA\OrbExceptions.h"\
	"..\..\..\src\org\omg\CORBA\portable\ApplicationException.h"\
	"..\..\..\src\org\omg\CORBA\portable\Delegate.h"\
	"..\..\..\src\org\omg\CORBA\portable\InputStream.h"\
	"..\..\..\src\org\omg\CORBA\portable\InvokeHandler.h"\
	"..\..\..\src\org\omg\CORBA\portable\ObjectImpl.h"\
	"..\..\..\src\org\omg\CORBA\portable\OutputStream.h"\
	"..\..\..\src\org\omg\CORBA\portable\RemarshalException.h"\
	"..\..\..\src\org\omg\CORBA\portable\ResponseHandler.h"\
	"..\..\..\src\org\omg\PortableServer\POA.h"\
	"..\..\..\src\org\omg\PortableServer\POAManager.h"\
	"D:\programr\lang\c++\ide\BCB5\CBuilder5\Include\string.h"\
	
# End Source File
# End Target
# End Project
