// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/System.h,v 1.47 2005/04/28 15:00:04 kommer Exp $

#ifndef acdk_lang_System_h
#define acdk_lang_System_h

#include "InOutPreDeclaration.h"

#include "ObjectArrayImpl.h"
#include "sys/ObjectHeap.h"
#include "sys/BackTrace.h"
#include "sys/core_system.h"

#include "Thread.h"
#include "ArrayIndexOutOfBoundsException.h"
#include "NullPointerException.h"

#include <acdk/util/Properties.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/io/InputReader.h>

#include <errno.h>

namespace acdk {
  namespace util {
    ACDK_DECL_INTERFACE(Map);
    ACDK_DECL_CLASS(AbstractMap);
    ACDK_DECL_CLASS(HashMap);
    ACDK_DECL_CLASS(Properties);
  } // util
} // acdk


namespace acdk {
namespace lang {

/**
   Represent platform flags
*/
enum PlatformFlags
{
  PfWin32      = 0x00000001,
  PfUnix       = 0x00000002,
  PfLinux      = 0x00000012,
  PfFreeBSD    = 0x00000022,
  PfSolaris    = 0x00000042,
  PfCygwin     = 0x00000082,

  PfCcGcc      = 0x00100000,
  PfCcVc       = 0x00200000,
  PfCcBcc      = 0x00400000,

  Pf32Bit      = 0x10000000,
  Pf64Bit      = 0x20000000,

  PfBigEndian  = 0x40000000,
  PfLittleEndian  = 0x80000000

};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, PlatformFlags);


/**
  this flags combined signal in which initialization level 
  ACDK is
*/
enum SystemStatus
{
  /**
    The ACDK main function not entered
  */
  SystemStatusBeforeMain =  0x00000000,
  /**
    System::main is called, but not yet initialized
  */
  SystemStatusStartMain   = 0x00000001,
  /**
    the configuration (environment, properties, command line)
    is read
  */
  SystemStatusConfigLoaded  = 0x00000002,
  /**
    currently running in users code
  */
  SystemStatusInUsersMain      = 0x00000010,
  /**
    users main finished
    make some cleanup in System::main
  */
  SystemStatusAfterUsersMain   = 0x00000100,
  /**
    ACDK quit main, code probably called by
    global destructors
  */
  SystemStatusAfterMain        = 0x00001000
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SystemStatus);

ACDK_DECL_INTERFACE(Initializer);

/** 
  Used to be registered into main
  to enable initialization and deinitialization
  before user defined main starts and or 
  after user defined main ends
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.47 $
  @date $Date: 2005/04/28 15:00:04 $
*/  

class ACDK_CORE_PUBLIC Initializer 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Initializer)
public:
  virtual void beforeMain() = 0;
  virtual void afterMain() = 0;
};


class ArgumentHolder; 

/** 
  Signature of the user declared, java compatible main-Function. 
*/
typedef int (*ACDK_MainFunc)(RObjectArrayImpl<RString> args);

ACDK_DECL_CLASS(SystemImpl);

/** 
  @see acdk::lang::System

  API: Java & ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.47 $
  @date $Date: 2005/04/28 15:00:04 $
*/  
class ACDK_CORE_PUBLIC SystemImpl
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(SystemImpl)
protected:

  /** all current loaded properties */
  acdk::util::RProperties _properties;
  /** Only the properties set by environment */    
  acdk::util::RProperties _environment;
  acdk::util::RPropertiesChangeListener _systemPropertiesListener;
  /** represents the main-thread */
  RThread _mainThread;
  /**
    path list with ACDK libraries
  */
  RStringArray _acdkPath;
  /**
    contains the arguments including general acdk options
  */
  RStringArray _unfilteredArgs;
  /**
    contains the arguments passed to application
  */
  RStringArray _filteredArgs;
  /** 
  protected constructors, call 
  int System::main(ACDK_MainFunc mainfunc, int argc, char* argv[]); 
  to initialize ACDK
  */
  SystemImpl();
  foreign SystemImpl(int argc, char* argv[], char** envptr = 0);
public:
  foreign virtual RStringArray init(int argc, char* argv[], char** envptr, IN(RStringArray) args);
  /*  static void init_io(); obsolet */
  /** unitialize System. */
  virtual void deinit();
  /** reloads config */
  virtual void reinit();
  foreign virtual ~SystemImpl();
  acdk::util::RProperties properties();
  acdk::util::RProperties environment();
  /**
    see System::getAcdkPath()
  */
  RStringArray getAcdkPath();
  /** 
    overwrite the given properties 
  */
  void setProperties(IN(acdk::util::RProperties) props);
  /**
    return the arguments given to the users main function
  */
  RStringArray getFilteredArgs() { return _filteredArgs; }
  /**
    return the arguments before filtering general ACDK options
  */
  RStringArray getUnfilteredArgs() { return _unfilteredArgs; }

  void setFilteredArgs(IN(RStringArray) args) { _filteredArgs = args; }
protected:
  foreign void _loadEnvProperties(char** envptr = 0);
  void _loadAppProperties();
  /** filters away acdk-specific arguments.
    -acdk-home=pathtoacdk
  */
  RStringArray _filterArgs(IN(RStringArray) args);
  foreign ArgumentHolder* _argumentHolder;
  friend class System;
};

// needed in metainfo
ACDK_DECL_CLASS(System);

/** 
  Represends the host running this programm.
  API: Java & ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.47 $
  @date $Date: 2005/04/28 15:00:04 $
*/  
class System 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO2(System)
public:
  foreign ACDK_CORE_PUBLIC virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } 
  foreign ACDK_CORE_PUBLIC static ::acdk::lang::dmi::ClazzInfo* clazzInfo();
  foreign ACDK_CORE_PUBLIC static ::acdk::lang::RClass GetClass(); 
  foreign ACDK_CORE_PUBLIC virtual void getCollectableFields(FieldReferences& fields); 
private:   
  foreign friend class SystemImpl;
private:
  foreign static void _saveStackBase(unsigned int SP); // this should be friend of Thread, but include-chain disallows it
public:
  foreign static unsigned int getStackBase();
#if 0
  /** which type of Heap-Management should be used  
    @deprecated 
  */
    
  enum DefaultHeapType
  {
    /** 
      command line option -acdk-pa
      Using the full features PagedAllocator Heap 
    */
    PA_Heap = 0,
    /** 
      command line option -acdk-rcgc
      Using the conservativ but slower full featured ReferenceCounter & Garbage Collector Heap
    */
    RC_GC_Heap = 1,
    /** 
      command line option -acdk-rc
      Only Reference Counter no GC, no debugging, but quite fast */
    RC_Heap = 2,
    /** experimentel Boehm Garbage Collector */
    GC_Heap = 3
  };
#endif // 0
  
  /**
    return  a combination of SystemStatus flags
  */
  ACDK_CORE_PUBLIC static OUT(int) getSystemStatus();

#if defined(ACDK_OS_WIN32)
  foreign ACDK_CORE_PUBLIC static bool _unixMode;
#endif 

  ACDK_CORE_PUBLIC static bool hasConsole;
  /**
    @internal
  */
  foreign ACDK_CORE_PUBLIC static sys::ObjectHeap::HeapType defaultHeapType;
  /**
    The standard character based in reader
  */
  ACDK_CORE_PUBLIC static acdk::io::RInputReader in;
  /**
    The standard character base out writer
  */
  ACDK_CORE_PUBLIC static acdk::io::RPrintWriter out;
  /**
    The standard character base error writer
  */
  ACDK_CORE_PUBLIC static acdk::io::RPrintWriter err;
  /**
    Calls ::exit() after deinitialization of System
    Normally you should not call this method
  */
  ACDK_CORE_PUBLIC static void exit(int status);
  /**
    call the garbage collector
  */
  ACDK_CORE_PUBLIC static bool gc();
  /**
    return a counter in milli seconds
  */
  ACDK_CORE_PUBLIC static jlong currentTimeMillis();
  /**
    copy an array

    @throw NullPointerException if one array is nil
    @throw ArrayIndexOutOfBoundsException if a index position is invalid
  */
  template <class T>
  template_static
  void 
  arraycopy(IN(RBasicArray<T>) src, int srcpos, IN(RBasicArray<T>) dst, int dstpos, int length)
  {
    if (src == Nil || dst == Nil)
      THROW0(NullPointerException);
    
    if ((srcpos < 0 || srcpos + length > src.length()) ||
      (dstpos < 0 || dstpos + length > dst.length()) ||
      (length < 0)) {
      THROW0(ArrayIndexOutOfBoundsException);
    }
    int di = dstpos;
    int si = srcpos;
    int i = 1;
    if (src == dst && srcpos < dstpos ) {// overlapping copy
      i=-1;
      di += (length-1);
      si += (length-1);
    }
    for (; length > 0; di+=i, si+=i, length--)
      dst[di] = src[si];
  }
  /**
    copy an array

    @throw NullPointerException if one array is nil
    @throw ArrayIndexOutOfBoundsException if a index position is invalid
  */
  template <class T>
  template_static
  void 
  arraycopy(IN(RObjectArrayImpl<T>) src, int srcpos, IN(RObjectArrayImpl<T>) dst, int dstpos, int length)
  {
    if (src == Nil || dst == Nil)
      THROW0(NullPointerException);
    
    if ((srcpos < 0 || srcpos + length > int(src.length())) ||
      (dstpos < 0 || dstpos + length > int(dst.length())) ||
      (length < 0)) {
      THROW0(ArrayIndexOutOfBoundsException);
    }
    int di = dstpos;
    int si = srcpos;
    int i = 1;
    if (src == dst && srcpos < dstpos ){ // overlapping copy
      i=-1;
      di += (length-1);
      si += (length-1);
    }
    for (; length > 0; di+=i, si+=i, length--)
      dst[di] = src[si];
  }
  /**
    @internal
    copy an array

    @throw NullPointerException if one array is nil
    @throw ArrayIndexOutOfBoundsException if a index position is invalid
  */
  template <class T>
  template_static
  void
  arraycopy(const T* src, int srcpos, IN(RBasicArray<T>) dst, int dstpos, int length)
  {
    if (src == 0 || dst == Nil)
    THROW0(NullPointerException);
  
    if ((srcpos < 0) ||
        (dstpos < 0 || dstpos + length > int(dst.length())) ||
        (length < 0)) {
      THROW0(ArrayIndexOutOfBoundsException);
    }
    memmove(dst->data() + dstpos, src + srcpos, length);
  }
  

  /** 
    @internal
    this group of functions is for registering references as static reference 
    refer to acdk::lang::sys::ObjectHeap::registerStaticReference for more information
  */
  template <class T>
  template_static
  const RefHolder<T>&
  registerStaticReference(RefHolder<T>& obj)
  {
    sys::ObjectHeap::registerStaticReference(reinterpret_cast<RefHolder<Object>*>( const_cast<RefHolder<T>&>(obj)._ref_this()));
    return obj;
  }
  template <class T>
  template_static
  const InterfaceHolder<T>&
  registerStaticReference(InterfaceHolder<T>& obj)
  {
    sys::ObjectHeap::registerStaticReference(reinterpret_cast<RefHolder<Object>*>( const_cast<InterfaceHolder<T>&>(obj)._ref_this()));
    return obj;
  }
  
  /** 
    @internal
    emulating obj pointer as static reference.
    Heap holds reference.
  */
  foreign ACDK_CORE_PUBLIC static void registerStaticReference(Object* obj);
  /**
    Wrapper translating c-main to ACDK/Java main.
    Use this, before calling any further calls to ACDK, because inside this
    wrapper some initialisation for the ACDK will be done.
    @param mainfunc Function pointer to function like this:
    @code
    class MyClass
    {
    public:
      static int main(RStringArray args);
    };
    @endcode
    @param argc argc given from c-main
    @param argv argv given from c-main
    @param envptr envptr given from c-main or 0
    @param addargs additionally arguments, terminated with 0
    @return exit status of the process
  */
  foreign ACDK_CORE_PUBLIC static int main(ACDK_MainFunc mainfunc, int argc, char* argv[], char** envptr = 0
                                   , const char** addargs = 0);
  /**
    do the real work
  */
  foreign ACDK_CORE_PUBLIC static int main2(ACDK_MainFunc mainfunc, int argc, char* argv[], char** envptr, const char** addargs);

#if defined(ACDK_OS_WIN32)
  foreign ACDK_CORE_PUBLIC static int main(ACDK_MainFunc mainfunc, HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                                           int nCmdShow, const char** addargs = 0);
#endif // defined(ACDK_OS_WIN32)

  /**
    @internal
    In case an acdk library will be loaded at runtime without initialize
    acdk with the main function, this method should be called
    to initialize the acdk libries.
  */
  ACDK_CORE_PUBLIC static void initAsSharedLibrary();
  /** 
    @internal
  */
  ACDK_CORE_PUBLIC static void deInitAsSharedLibrary();

  /** Api: ACDK
   *  Returns the argc value given to function
   *  int main(int argc, char* argv[], char** envptr).
   *  All acdk specific arguments are removed.  
  */
  ACDK_CORE_PUBLIC static OUT(int) getArgc();
  /** Api: ACDK
   *  Returns the argv list given to function
   *  int main(int argc, char* argv[], char** envptr).
   *  All acdk specific arguments are removed.  
  */
  foreign ACDK_CORE_PUBLIC static char** getArgv();
  /** Api: ACDK
   *  Returns the original argc value given to function
   *  int main(int argc, char* argv[], char** envptr). 
  */
  ACDK_CORE_PUBLIC static OUT(int) getOriginalArgc();
  /** Api: ACDK
   *  Returns the original argv list given to function
   *  int main(int argc, char* argv[], char** envptr). 
  */
  foreign ACDK_CORE_PUBLIC static char** getOriginalArgv();
  
  /**
    @internal
    return the global System object
  */
  ACDK_CORE_PUBLIC static RSystemImpl getSystem();
  /**
    return the arguments passed to ACDK application main
    The internal options, filtered by ACDK or other library
    used for initialization are not contained in this arguments.
    Use getUnfilteredArgs() to receive all arguments
  */
  ACDK_CORE_PUBLIC static RStringArray getArguments();
  /**
    return all arguments, including internal options
  */
  ACDK_CORE_PUBLIC static RStringArray getUnfilteredArguments();
  /**
    return the system properties
  */
  ACDK_CORE_PUBLIC static acdk::util::RProperties getProperties();
  /**
    return the system environment
  */
  ACDK_CORE_PUBLIC static acdk::util::RProperties getEnvironment();
  /**
    return the property value of given key
    @see acdk::util::Properties
  */
  ACDK_CORE_PUBLIC static RString getProperty(IN(RString) key);
  /**
    return the property value of given key
    @see acdk::util::Properties
  */
  ACDK_CORE_PUBLIC static RString getProperty(IN(RString) key, IN(RString) def);
  /**
    set the system properties
    @see acdk::util::Properties
  */
  ACDK_CORE_PUBLIC static void setProperties(IN(acdk::util::RProperties) props);
  /**
    set the property value of given key
    @see acdk::util::Properties
  */
  ACDK_CORE_PUBLIC static RString setProperty(IN(RString) key, IN(RString) value);
  /**
    set system error writer
  */
  ACDK_CORE_PUBLIC static void setErr(IN(acdk::io::RPrintWriter) newerr);
  /**
    set system input reader
  */
  ACDK_CORE_PUBLIC static void setIn(IN(acdk::io::RInputReader) newin);
  /**
    set system standard output writer
  */
  ACDK_CORE_PUBLIC static void setOut(IN(acdk::io::RPrintWriter) newout);
  /**
    @return the fully qualified executable name
  */
  ACDK_CORE_PUBLIC static RString getModulePath();
  /**
    @return the base name of the executable without extension
    @param stripExt return without .exe suffix on windows
  */
  ACDK_CORE_PUBLIC static RString getModuleName(bool stripExt = true);
  /**
    @return the relativ directory of the module 
  */
  ACDK_CORE_PUBLIC static RString getModuleDir();
  /**
    return the path of acdk home
    evaluates -acdk-home of command line or ACDKHOME or ACDK_HOME of environmnet
  */
  ACDK_CORE_PUBLIC static RString getAcdkHome();
  /**
    return the path of acdk tools home
    evaluates -acdk--tools-home of command line or ACDK_TOOLS_HOME of environmnet
    if not set return the parent-parent directory of the executable
    (/acdk/bin/myexecutable -> /acdk is ACDK_TOOLS_HOME)
  */
  ACDK_CORE_PUBLIC static RString getAcdkToolsHome();
  /**
    returns the list of directories in the PATH enviromnet variable
  */
  ACDK_CORE_PUBLIC static RStringArray getEnvPath();
  /**
    return the path, where to look after shared libraries.
    See also 
  */
  ACDK_CORE_PUBLIC static RStringArray getAcdkPath();
  /**
    add dir at end of path list
  */
  ACDK_CORE_PUBLIC static void addAcdkPath(IN(RString) dir);
  /*
    insert dir at beginning of path list
  */
  ACDK_CORE_PUBLIC static void insertAcdkPath(IN(RString) dir);
  /**
    Print the current callstack of the current thread
  */
  ACDK_CORE_PUBLIC static void printStackTrace(int ignoreFromTop = 0);
  /**
    Print the current callstack of the current thread
  */
  ACDK_CORE_PUBLIC static void printStackTrace(IN(acdk::io::RPrintWriter) out, int ignoreFromTop = 0);
  /**
    @internal
    Print the current callstack of the current thread
  */
  ACDK_CORE_PUBLIC static void printStackTrace(IN(acdk::io::RPrintWriter) out, IN(RStackFrameArray) stackTrace, int ignoreFromTop = 0);
  /**
    return just the address of obj casted to an int
  */
  ACDK_CORE_PUBLIC static int identityHashCode(IN(RObject) obj);
  /** 
    re-initialize System
    In normal case load Config-Files etc.
    API: ACDK<br>
    */
  ACDK_CORE_PUBLIC static void reinit();
  /**
    @internal
    at system initialization call the registerd inizializer
  */
  foreign ACDK_CORE_PUBLIC static void registerForInitialization(Initializer* inizializer);
  /**
    Checks if current execution frame is valide, which means inside System::main 
  */
  ACDK_CORE_PUBLIC static bool isInMain()
  {
    return (getSystemStatus() & SystemStatusStartMain) != 0;
  }
  /**
    return true if currently running inside users main function
    @see SystemStatus
  */
  ACDK_CORE_PUBLIC static bool isInUserMain()
  {
    return (getSystemStatus() & SystemStatusInUsersMain) != 0;
  }
  /** 
    returns true if basic configuration is loaded
    @see SystemStatus
  */
  ACDK_CORE_PUBLIC static bool configurationLoaded()
  {
    return (getSystemStatus() & SystemStatusConfigLoaded) != 0;
  }
  /**
    return true if users main function leaved
    @see SystemStatus
  */
  ACDK_CORE_PUBLIC static bool afterUsersMain()
  {
    return (getSystemStatus() & SystemStatusAfterUsersMain) != 0;
  }
  /**
    return true if Systems main function leaved
    @see SystemStatus
  */
  ACDK_CORE_PUBLIC static bool afterMain()
  {
    return (getSystemStatus() & SystemStatusAfterMain) != 0;
  }
  
  /**
    return information about last system-error.
  */
  ACDK_CORE_PUBLIC static int getErrno();
  /**
    set system errno variable to 0
  */
  ACDK_CORE_PUBLIC static void resetErrno();
  /** 
    return last erno message 
  */
  ACDK_CORE_PUBLIC static RString getErrmsg();
  /** 
    return stringified erno message for a given errnumber
  */
  ACDK_CORE_PUBLIC static RString getErrmsg(int errnumber);

  /** 
    equal to getErrmsg on unix, GetLastError on windows 
  */
  ACDK_CORE_PUBLIC static RString getLastError();
  /** 
    return the help for command line options parsed by System::main 
  */
  ACDK_CORE_PUBLIC static RString getSystemCmdLineOps();
  /**
    load given library
    @see acdk::lang::SharedLibrary
  */
  ACDK_CORE_PUBLIC static void loadLibrary(IN(RString) libname);

  /**
    return the overal limit of memory usage
  */
  ACDK_CORE_PUBLIC static jlong getMaxMemoryUsage();
  /**
    limit overall usage of memory.
    If maximum memory is consumed in this process, ACDK try call the garbage collector.
    If after garbage collection still not enough (below given value) memory is available 
    a OutOfMemoryException will be thrown.
  */
  ACDK_CORE_PUBLIC static void setMaxMemoryUsage(jlong maxmem);
  /**
    return the limit of memory usage inside this thread
  */
  ACDK_CORE_PUBLIC static jlong getThreadMaxMemoryUsage();
  /**
    same as setMaxMemoryUsage() but on per thread basis
  */
  ACDK_CORE_PUBLIC static void setThreadMaxMemoryUsage(jlong maxmem);
  /**
     returns a combination of PlatformFlags
  */
  ACDK_CORE_PUBLIC static int getPlatformFlags();
  /**
    return all known Object instances, which are not 
    references as members by other objects.
  */
  ACDK_CORE_PUBLIC static RObjectArray getRootObjects(bool includeStaticReferences = true);
  /**
    @param flags combination of ListObjectsFlags
  */
  ACDK_CORE_PUBLIC static RObjectArray getObjectList(int flags);
private:
 
  ACDK_CORE_PUBLIC static RSystemImpl _gSystem;
  foreign ACDK_CORE_PUBLIC static int _argc;     // Command line options.
  foreign ACDK_CORE_PUBLIC static char** _argv;  // Command line options.
#if !defined(DOXYGENONLY)
  foreign  struct _initList_t {
    Initializer* cl;
    _initList_t* next;
  };
  /// @internal
  foreign ACDK_CORE_PUBLIC static _initList_t _initList;
#endif //!defined(DOXYGENONLY)
};

#if !defined(DOXYGENONLY)


/**
  @internal
*/
template <class T>
class SystemInitializer 
{
public: 
  SystemInitializer() 
  {
    Initializer* i = &_initClass;
    System::registerForInitialization(i);
  }
private:
  T _initClass;
};
#endif //!defined(DOXYGENONLY)


} // namespace lang
} // namespace acdk

#include "sys/StaticObjectWrapper.h"

#endif //acdk_lang_System_h

