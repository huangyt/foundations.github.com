// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
//
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
//
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/System.cpp,v 1.97 2005/05/02 23:09:42 kommer Exp $


#include <acdk.h>

#include <stdlib.h>

#ifdef HAS_UNISTD_H
#  include <unistd.h>
#endif

#include "System.h"
#include "Runtime.h"
#include "ThreadLocal.h"
#include "StackOverflowError.h"
#include "ArithmeticException.h"

#include "sys/core_value_scope.h"
#include <acdk/util/Vector.h>
#include <acdk/lang/Long.h>
#include <acdk/util/Properties.h>
#include <acdk/util/Locale.h>
#include <acdk/io/ConsoleReader.h>
#include <acdk/io/ConsoleWriter.h>
#include <acdk/io/ConsoleCharWriter.h>
#include <acdk/io/ConsoleCharReader.h>
#include <acdk/util/Date.h>
#include <acdk/util/StringTokenizer.h>
#include <acdk/util/logging/LogManager.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/NullWriter.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/util/Vector.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/File.h>
#include <acdk/locale/UCS2Encoding.h>

#include "SystemIntern.h"

#if defined(ACDK_USE_SYNC_SIGNALS)
# include "signal.h"
#endif

namespace acdk {
namespace lang {



//using namespace acdk::io;
//using namespace acdk::util;

using sys::eofl;
using sys::coreout;

USING_CLASS(acdk::util::, Properties);
USING_CLASS(acdk::io::, ConsoleReader);
USING_CLASS(acdk::io::, ConsoleWriter);
USING_CLASS(acdk::io::, InputReader);
USING_CLASS(acdk::io::, PrintWriter);
USING_CLASS(acdk::io::, File);
USING_CLASS(acdk::io::, FileReader);


acdk::io::RInputReader System::in;
acdk::io::RPrintWriter System::out;
acdk::io::RPrintWriter System::err;
int System::_argc = 0;
char** System::_argv = 0;

OUT(int) 
System::getSystemStatus()
{
  static int _systemStatus = 0;
  return _systemStatus;
}

#if defined(ACDK_OS_WIN32)
bool System::_unixMode = false;
#endif
bool System::hasConsole = true; 

//static
sys::ObjectHeap::HeapType System::defaultHeapType = sys::ObjectHeap::PA_Heap;
//sys::ObjectHeap::HeapType System::defaultHeapType = sys::ObjectHeap::RC_Heap;

SystemImpl::SystemImpl()
: Object()
, _properties(new acdk::util::Properties())
, _environment(new acdk::util::Properties())
, _argumentHolder(0)
{

}

#if !defined(DOXYGENONLY)
/// @internal
class SystemPropertiesListener
: extends acdk::lang::Object
, implements acdk::util::PropertiesChangeListener
{
public:
  SystemPropertiesListener() {}
  virtual void propertyChanged(acdk::util::PropertiesChangeAction action, IN(acdk::util::RProperties) props, IN(RString) key, IN(RObject) value) 
  {

  }
};
#endif //!defined(DOXYGENONLY)


SystemImpl::SystemImpl(int argc, char* argv[], char** envptr)
: Object()
, _properties(new Properties())
, _environment(new acdk::util::Properties())
, _systemPropertiesListener(new SystemPropertiesListener())
, _argumentHolder(0)
{
  RStringArray args = new StringArray(argc);
  int i;
  for (i = 0; i < argc; i++) {
    args[i] = SCS(argv[i]);
  }
  _argumentHolder = new ArgumentHolder(args);
  init(argc, argv, envptr, args);
}

//virtual
SystemImpl::~SystemImpl()
{
  if (_argumentHolder != 0) {
    delete _argumentHolder;
    _argumentHolder = 0;
  }
}

void
SystemImpl::setProperties(IN(acdk::util::RProperties) props)
{
  _properties = props;
}

//static 
RStringArray 
SystemImpl::getAcdkPath()
{
  if (_acdkPath != Nil)
    return _acdkPath;

  RString p = System::getProperty("ACDKPATH");
  if (p != Nil && p->length() > 0)
    _acdkPath = acdk::util::StringTokenizer(p, acdk::io::File::pathSeparator()).allToken();
  if (_acdkPath == Nil)
    _acdkPath = new StringArray(0);
  RString acdkhome = System::getAcdkHome();
  RString acdktoolshome = System::getAcdkToolsHome();
  if (_acdkPath->find(acdkhome) == -1)
    _acdkPath->insert(0, acdkhome);

  if (acdkhome->equals(acdktoolshome) == false)
  {
    _acdkPath->insert(0, acdktoolshome);
  }
  return _acdkPath;
}



//static
void
System::exit(int status)
{
  if (_gSystem != Nil)
    _gSystem->deinit();
  _gSystem = Nil;
  ::exit(status);
}

//static
bool
System::gc()
{
    // gc-ing only current thread
  return sys::ObjectHeap::gc(true);
}

//static
int&
System::getArgc()
{
  if (_gSystem->_argumentHolder == 0) 
  {
    static int retint = 0;
    return retint;
  }
  return _gSystem->_argumentHolder->getArgc();
}
//static
char**
System::getArgv()
{
  if (_gSystem->_argumentHolder == 0)
    return 0;
  return _gSystem->_argumentHolder->getArgv();
}
//static
int&
System::getOriginalArgc()
{
    return _argc;
}
//static
char**
System::getOriginalArgv()
{
    return _argv;
}


//static
RSystemImpl System::_gSystem = Nil;

//static
RSystemImpl
System::getSystem()
{
  if (System::_gSystem == Nil) {
    // acdk_core is loaded dynamically (through scripting)
    _gSystem = new SystemImpl();
    //int argc, char* argv[], char** envptr, RStringArray args
    static char* nullargv[] = { 0 };
#if defined(ACDK_OS_WIN32) && !defined(__MWERKS__) && !defined(ACDK_OS_CYGWIN32)
    _gSystem->init(0, nullargv, _environ, new StringArray(0));
#else
    _gSystem->init(0, nullargv, 0, new StringArray(0));
#endif
  }
  return _gSystem;
}

RString 
System::getSystemCmdLineOps()
{
  return
    "common acdk options:\n"
    "  -acdk-home=<pathname>   set the path to acdk home directory\n"
    "  -acdk-home <pathname>\n"
    "  -acdk-tools-home=<pathname>   set the path to acdk tools home directory\n"
    "  -acdk-tools-home <pathname>\n"
    "  -acdk-path <pathlist>   Env path list for ACDKPATH, to find acdk libraries\n"
    "  -Dkey=value             Define a entry in the System properties\n"
    "  -acdk-gc                run with mark/sweep garbage collection (experimental)\n"
    "  -acdk-rc                run with reference counting garbage collection\n"
    "  -acdk-rcgc              run with reference counting and mark/sweek gc\n"
    "  -acdk-pa                run with PageAllocator garbage collection (default)\n"
    "  -acdk-st                run in single thread mode\n"
    "  -acdk-sp                run on a single processor\n"
    "  -cygpath                on windows platform runs in cygwin enviromnent\n"
    "  -acdk-enc <char enc>    use encoding for console in/output\n"
    "  -acdk-maxmem <number>   limit memory usage -1 no limit, otherwise number of MB\n"
    "  -loglevel <loglevel>    integer [0(all)-65535(Non)] or \n"
    "                                  [All|Trace|Debug|Info|Warn|Note|Error|Fatal|None]\n"
    "                          if a following -logto option is given, the logger will be configured\n"
    "                          with this LogLevel.\n"
    "  -logcat <category>      Log only with category. a following -logto option will be configured\n"
    "                          with this category.\n"
    "  -logto <consumer>       log consumer indentifier or file.\n"
    "                          known identifier are [out|err|dbg]\n"
    ;
}

RStringArray
SystemImpl::_filterArgs(IN(RStringArray) arguments)
{
  const char* mhstr = "-acdk-home=";
  const char* mhstr2 = "-acdk-home";
  const char* mthstr = "-acdk-tools-home=";
  const char* mthstr2 = "-acdk-tools-home";
  const char* mapstr = "-acdk-path";
  const char* acdk_gc = "-acdk-gc";
  const char* acdk_rc = "-acdk-rc";
  const char* acdk_rcgc = "-acdk-rcgc";
  const char* acdk_st = "-acdk-st";
  const char* acdk_pa = "-acdk-pa"; // using PageAllocator instead of RC_GC_Heap
  const char* acdk_console_encoding = "-acdk-enc";
  const char* acdk_maxmem = "-acdk-maxmem";
  const char* cygpath = "-cygpath";
  const char* acdk_tmc = "-acdk-tmc"; // trace method call
  RStringArray args = arguments;
  if (args == Nil) // if loaded lately from another scripting host
    return new StringArray(0);
  
  for (int i = 0; i < args->length(); i++) 
  {
    bool removeelement = false;
    RString arg = args[i];
    if (arg->startsWith(mhstr) == true) 
    {
      RString mh = args[i]->substr(strlen(mhstr));
      System::getProperties()->setProperty("ACDKHOME", mh);
      removeelement = true;
    } 
    else if (arg->equals(mhstr2) == true) 
    {
      ++i;
      System::getProperties()->setProperty("ACDKHOME", args[i]);
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      removeelement = false;
    } 
    else if (arg->startsWith(mthstr) == true) 
    {
      RString mh = arg->substr(strlen(mhstr));
      System::getProperties()->setProperty("ACDK_TOOLS_HOME", mh);
      removeelement = true;
    } 
    else if (arg->equals(mthstr2) == true) 
    {
      ++i;
      System::getProperties()->setProperty("ACDK_TOOLS_HOME", args[i]);
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      removeelement = false;
    } 
    else if (arg->equals(mapstr) == true)
    {
      ++i;
      System::getProperties()->setProperty("ACDKPATH", args[i]);
       args = acdk::util::Arrays::removeElement(args, i);
      --i;
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      removeelement = false;
    }
    else if (arg->equals(acdk_st) == true || arg->equals("-acdk-sp") == true) 
    {
      ObjectBase::singleThreaded = true;
      if (arg->equals("-acdk-sp") == true)
        acdk::lang::sys::core_atomicop::singleProcessor = true;
      removeelement = true;
    } 
    else if (arg->equals(acdk_tmc) == true) 
    {
      Runtime::_traceMethodCalls = true;
      removeelement = true;
    } 
    else if (arg->equals(acdk_pa) == true ||
             arg->equals(acdk_gc) == true ||
             arg->equals(acdk_rc) == true ||
             arg->equals(acdk_rcgc) == true
      ) 
    {
      removeelement = true;
    }
    else if (arg->equals(acdk_console_encoding) == true)
    {
      ++i;
      System::getProperties()->setProperty("user.encoding", args[i]);
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      removeelement = false;
    }
    else if (arg->equals(acdk_maxmem) == true)
    {
      ++i;
      jlong num = acdk::lang::Long::parseLong(args[i]);
      if (num != -1)
        num = num * 1024 * 1024;
      System::setMaxMemoryUsage(num);
      System::setThreadMaxMemoryUsage(num);
      
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
      removeelement = false;
    }
    else if (arg->startsWith("-D") == true)
    {
      RString targ = arg->substr(2);
      int idx = targ->indexOf('=');
      if (idx != -1)
      {
        RString key = targ->substr(0, idx);
        RString value = targ->substr(idx + 1);
        System::setProperty(key, value);
        removeelement = true;
      }
    }
#if defined(ACDK_OS_WIN32)
    else if (arg->equals(cygpath) == true)
    {
      System::_unixMode = true;
      removeelement = true;
    }
#endif
    if (removeelement == true) 
    {
      args = acdk::util::Arrays::removeElement(args, i);
      --i;
    }
  }
  return acdk::util::logging::LogManager::parseCommandLine(args);
}

void 
_initAcdkHome()
{
  RString s = System::getProperty("ACDKHOME");
  if (s != Nil)
    return;
  s = System::getProperty("ACDK_HOME");
  if (s != Nil)
  {
    System::setProperty("ACDKHOME", s);
    return;
  }
  RString moddir = System::getModuleDir();
  moddir = acdk::io::File(moddir).getParentFile()->getCanonicalPath();
  System::setProperty("ACDKHOME", moddir);
}


//static 
RString 
System::getAcdkHome() 
{
  RString s = System::getProperty("ACDKHOME");
  if (s != Nil)
    return s;
  _initAcdkHome();
  s = System::getProperty("ACDKHOME");
  return s;
}

//static 
RString 
System::getAcdkToolsHome()
{
  RString s = System::getProperty("ACDK_TOOLS_HOME");
  if (s != Nil)
    return s;
  RString moddir = System::getModuleDir();
  moddir = acdk::io::File(moddir).getParentFile()->getCanonicalPath();
  System::setProperty("ACDK_TOOLS_HOME", moddir);
  return moddir;
  //return getAcdkHome();
}

//static 
RStringArray 
System::getAcdkPath()
{
  return getSystem()->getAcdkPath();
}

//static 
void 
System::addAcdkPath(IN(RString) dir)
{
   getSystem()->getAcdkPath()->append(dir);
}
  
//static 
void 
System::insertAcdkPath(IN(RString) dir)
{
  getSystem()->getAcdkPath()->insert(0, dir);
}

//virtual
RStringArray
SystemImpl::init(int argc, char* argv[], char** envptr, IN(RStringArray) arguments)
{
  System::_argc = argc;
  System::_argv = argv;
  _mainThread = new Thread(MainThreadType);
  _loadEnvProperties(envptr);
  
  _loadAppProperties();
  _unfilteredArgs = arguments;
  RStringArray args = _filterArgs(arguments);
  _filteredArgs = args;
  _initAcdkHome();
  
  //RConsoleReader cr = new ConsoleReader(ConsoleReader::Cin);
  //System::in = new InputReader((::acdk::io::RReader)cr);
    
  if (System::hasConsole == true)
    System::in = new InputReader((::acdk::io::RCharReader)new acdk::io::ConsoleCharReader());
  else
    System::in = new InputReader((::acdk::io::RCharReader)new acdk::io::StringReader(""));
  System::registerStaticReference(System::in);

  //System::registerStaticReference(System::in->getIn());
  //acdk::locale::REncoding uc2encoding = acdk::locale::UCS2Encoding::getUCS2Encoding();
  //System::out = new PrintWriter(new ConsoleWriter(ConsoleWriter::Cout), uc2encoding->getEncoder());
  if (System::hasConsole == true)
    System::out = new PrintWriter((::acdk::io::RCharWriter)new acdk::io::ConsoleCharWriter(acdk::io::CoutOutChannel));
  else
    System::out = new PrintWriter((::acdk::io::RWriter)new acdk::io::NullWriter());

  System::registerStaticReference(System::out);
  //System::registerStaticReference(System::out->getOut());

  //System::err = new PrintWriter(new ConsoleWriter(ConsoleWriter::Cerr), uc2encoding->getEncoder());
  if (System::hasConsole == true)
    System::err = new PrintWriter((::acdk::io::RCharWriter)new acdk::io::ConsoleCharWriter(acdk::io::CerrOutChannel));
  else
    System::err = new PrintWriter((::acdk::io::RWriter)new acdk::io::NullWriter());
  System::registerStaticReference(System::err);
  //System::registerStaticReference(System::err->getOut());
  System::getSystemStatus() |= SystemStatusConfigLoaded;
  return args;
}


//virtual
void
SystemImpl::deinit()
{
  Thread::removeSystemThread();
  Runtime::unregisterAllEventHandler(0);
  sys::ObjectHeap::clearAllStaticReferences();
  _mainThread = Nil;
}

//virtual
void
SystemImpl::reinit()
{
  RString moddir = System::getModuleDir();
  RString modname = System::getModuleName();
  File f(moddir, modname + ".cfg");
  if (f.exists() == false)
    return;
  RProperties def = (_properties == Nil ? RProperties(Nil) : _properties->defaults());
  FileReader fin(SR(File, f));
  _properties = new Properties(def);
  _properties->load((::acdk::io::RReader)SR(FileReader, fin));
  
}

//static
jlong
System::currentTimeMillis()
{
  return acdk::util::Date::getTickCount();
}


void
SystemImpl::_loadEnvProperties(char** envptr)
{
  if (envptr == 0)
    return;
  for (; *envptr != 0; envptr++) {
    RString tstr = SCS(*envptr);
    int fpos = -1;
    if ((fpos = tstr->indexOf(RString("="))) != -1) {
      if (tstr->length() > fpos + 1)
      {
        _properties->setProperty(tstr->substring(0, fpos), tstr->substring(fpos + 1));
        _environment->setProperty(tstr->substring(0, fpos), tstr->substring(fpos + 1));
      }
      else
      {
        _properties->setProperty(tstr->substring(0, fpos), "");
        _environment->setProperty(tstr->substring(0, fpos), "");
      }
    }
  }
}

void
SystemImpl_initStandardPropertyValues(IN(RProperties) props)
{
  /* from java
    java.version Java Runtime Environment version 
java.vendor Java Runtime Environment vendor 
java.vendor.url Java vendor URL 
java.home Java installation directory 
java.vm.specification.version Java Virtual Machine specification version 
java.vm.specification.vendor Java Virtual Machine specification vendor 
java.vm.specification.name Java Virtual Machine specification name 
java.vm.version Java Virtual Machine implementation version 
java.vm.vendor Java Virtual Machine implementation vendor 
java.vm.name Java Virtual Machine implementation name 
java.specification.version Java Runtime Environment specification version 
java.specification.vendor Java Runtime Environment specification vendor 
java.specification.name Java Runtime Environment specification name 
java.class.version Java class format version number 
java.class.path Java class path 
java.library.path List of paths to search when loading libraries 

  java.compiler Name of JIT compiler to use 
java.ext.dirs Path of extension directory or directories 

user.dir User's current working directory 
*/
  props->setProperty("acdk.vm.version", Integer::toString(ACDK_VERSION_NUMBER));
  props->setProperty("acdk.vm.vendor", "Ing.Buero Kommer, Artefaktur");
  props->setProperty("acdk.vm.name", ACDK_VERSION_STRING);

#if defined(ACDK_OS_WIN32)
  props->setProperty("acdk.io.tmpdir", System::getProperty("TEMP"));
  props->setProperty("user.name", System::getProperty("USERNAME"));
  props->setProperty("user.home", System::getProperty("HOMEDRIVE") + System::getProperty("HOMEPATH"));
  props->setProperty("line.separator", "\n\r");
  props->setProperty("os.name", System::getProperty("OS"));
  props->setProperty("os.arch", System::getProperty("PROCESSOR_ARCHITECTURE"));
  
#else // asuming unix
  props->setProperty("acdk.io.tmpdir", System::getProperty("TMP"));
  props->setProperty("user.name", System::getProperty("LOGNAME"));
  props->setProperty("user.home", System::getProperty("HOME"));
  props->setProperty("line.separator", "\n");
  props->setProperty("os.name", System::getProperty("OSTYPE"));
  props->setProperty("os.arch", System::getProperty("HOSTTYPE"));
#endif
  // user.dir will not set, use File::getCWD()
  
  props->setProperty("file.separator", File::separator());
  props->setProperty("path.separator", File::pathSeparator());

  RString encoding = "ASCII";
  RString language = "en";
  RString country = "US";
  acdk::util::Locale::getSystemLocaleValues(language, country, encoding);
  // we have to do this, otherwise Double/Float::toString() doesn't work like expected
  acdk::util::Locale::setSystemLocale(acdk::util::Locale::getUS(), acdk::util::SysLocaleNumberic);
  props->setProperty("user.language", language);
  props->setProperty("user.region", country);
  props->setProperty("user.variant", "");
  props->setProperty("user.encoding", encoding);
}

void
SystemImpl_loadAcdkProperties()
{
  RString cfgfile = System::getAcdkToolsHome() + File::separator() + "cfg" + File::separator() + "acdk.cfg";
  File f(cfgfile);
  if (f.exists() == false)
    return;
  FileReader fin(SR(File, f));
  RProperties props = new Properties(System::getProperties());
  SystemImpl_initStandardPropertyValues(props);
  props->load((::acdk::io::RReader)SR(FileReader, fin));
  System::setProperties(props);
}

void
SystemImpl::_loadAppProperties()
{
  SystemImpl_loadAcdkProperties();
  RString moddir = System::getModuleDir();
  RString modname = System::getModuleName();
  File tf(moddir, modname + ".cfg");
  if (tf.exists() == false)
  {
    if (modname->endsWith("_r") == true || modname->endsWith("_d") == true)
      modname = modname->substr(0, modname->length() - 2);
    else
      return;
  }
  File f(moddir, modname + ".cfg");
  if (f.exists() == false)
    return;

  FileReader fin(SR(File, f));
  _properties = new Properties(_properties);
  _properties->load((::acdk::io::RReader)SR(FileReader, fin));
  _systemPropertiesListener = new SystemPropertiesListener();
  _properties->addPropertyChangeListener(_systemPropertiesListener);
}

//static
RProperties
System::getProperties()
{
  return System::getSystem()->properties();
}

//static 
RStringArray 
System::getArguments()
{
  return System::getSystem()->getFilteredArgs();
}

//static 
RStringArray 
System::getUnfilteredArguments()
{
  return System::getSystem()->getUnfilteredArgs();
}

//static 
acdk::util::RProperties 
System::getEnvironment()
{
  return System::getSystem()->environment();
}

RProperties
SystemImpl::properties()
{
  return _properties;
}

RProperties
SystemImpl::environment()
{
  return _environment;
}



//static
RString
System::getProperty(IN(RString) key)
{
  return getProperties()->getProperty(key);
}

//static
RString
System::getProperty(IN(RString) key, IN(RString) def)
{
  return getProperties()->getProperty(key, def);
}

//static
void
System::reinit()
{
  RSystemImpl sys = getSystem();
  sys->reinit();
}

//static
void
System::setProperties(IN(RProperties) props)
{
  RSystemImpl sys = getSystem();
  sys->setProperties(props);
}

//static
RString
System::setProperty(IN(RString) key, IN(RString) value)
{
  return RString(getProperties()->setProperty(key, value));
}

//static
void
System::setErr(IN(acdk::io::RPrintWriter) newerr)
{
  System::err = newerr;
  registerStaticReference(System::err);
  //registerStaticReference(System::err->getOut());
}

//static
void
System::setIn(IN(acdk::io::RInputReader) newin)
{
  System::in = newin;
  registerStaticReference(System::in);
  //registerStaticReference(System::in->getIn());
}

//static
void
System::setOut(IN(acdk::io::RPrintWriter) newout)
{
  System::out = newout;
  registerStaticReference(System::out);
  //registerStaticReference(System::out->getOut());
}

void
prefilterArgs(int argc, char* argv[], char** envptr, const char** addargs)
{
  sys::ObjectHeap::HeapType oldHeapType = System::defaultHeapType;

  for (int i = 1; i < argc; i++) 
  {
    if (strcmp(argv[i], "-acdk-pa") == 0)
      System::defaultHeapType = sys::ObjectHeap::PA_Heap;
    else if (strcmp(argv[i], "-acdk-rc") == 0)
      System::defaultHeapType = sys::ObjectHeap::RC_Heap;
    else if (strcmp(argv[i], "-acdk-rcgc") == 0)
      System::defaultHeapType = sys::ObjectHeap::RC_GC_Heap;
    else if (strcmp(argv[i], "-acdk-gc") == 0)
      System::defaultHeapType = sys::ObjectHeap::GC_Heap;
  }
  if (System::defaultHeapType != oldHeapType)
  {
    sys::ObjectHeap::pushFrame(System::defaultHeapType, sys::HeapIsThread, "MainHeap");
  }
}

namespace sys {
  
  void cleanup_system_objects();
} // namespace sys
// this is a temp-value while system is initializing itself
static unsigned int __SP = 0;

void exit_func()
{
  if (::acdk::lang::sys::core_system::getState() == ::acdk::lang::sys::InMain)
    sys::coreout << "Process terminating via exit" << sys::eofl;
  ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::AfterMain);
  System::getSystemStatus() &= ~(SystemStatusStartMain | SystemStatusInUsersMain);
  System::getSystemStatus() |= SystemStatusAfterUsersMain | SystemStatusAfterMain;
  
}
void terminate_func()
{
  ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::AfterMain);
  System::getSystemStatus() &= ~(SystemStatusStartMain | SystemStatusInUsersMain);
  System::getSystemStatus() |= SystemStatusAfterUsersMain | SystemStatusAfterMain;
  sys::coreout << "Process terminating via terminate" << sys::eofl;
}

#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
LONG ExceptionFilter(LPEXCEPTION_POINTERS pep) 
{
  if (pep->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION 
      //&& pep->ExceptionRecord->ExceptionInformation[0] == 0   // try to read (1 would be write)
      //&& pep->ExceptionRecord->ExceptionInformation[1] == 0 // address 0x0000000
      )
  {
    StringBuffer sb;
    sb << "NullPointer exception while try to ";
    if (pep->ExceptionRecord->ExceptionInformation[0] == 0)
      sb << "read ";
    else
      sb << "write ";
    sb << "at address " << Integer::toHexString(pep->ExceptionRecord->ExceptionInformation[1]);
    RNullPointerException ex = new NullPointerException(sb.toString());
    ex->_stackFrame._save_pcs(*pep->ContextRecord);
    throw ex;
  }
  else if (pep->ExceptionRecord->ExceptionCode == EXCEPTION_INT_DIVIDE_BY_ZERO ||
           pep->ExceptionRecord->ExceptionCode == EXCEPTION_FLT_DIVIDE_BY_ZERO)
  {
    RArithmeticException ex = new ArithmeticException("Divide by zero");
    ex->_stackFrame._save_pcs(*pep->ContextRecord);
    throw ex;
  }
  else if (pep->ExceptionRecord->ExceptionCode == EXCEPTION_STACK_OVERFLOW)
  {
    RStackOverflowError ex = new StackOverflowError("stack overflow");
    ex->_stackFrame._save_pcs(*pep->ContextRecord);
    throw ex;
  }

  return EXCEPTION_CONTINUE_SEARCH;
}
#endif

#if defined(ACDK_USE_SYNC_SIGNALS)

void sigsegv_signal_handler(int sig)
{
  printf("sigsegv_signal_handler\n");
  THROW0(NullPointerException);
}

#endif

int
System::main(ACDK_MainFunc mainfunc, int argc, char* argv[], char** envptr, const char** addargs/* = 0*/)
{
#if defined(ACDK_USE_SYNC_SIGNALS)
  signal(SIGSEGV, (sighandler_t)&sigsegv_signal_handler);
#endif
#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  __try {
#endif
  int ret = main2(mainfunc, argc, argv, envptr, addargs);
  return ret;
#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  } 
  __except(ExceptionFilter(GetExceptionInformation())) 
  {
    
  }
  return -1;
#endif
}
int
System::main2(ACDK_MainFunc mainfunc, int argc, char* argv[], char** envptr, const char** addargs/* = 0*/)
{
  sys::core_setbit_scope<int> _inMain(getSystemStatus(), SystemStatusStartMain);
  sys::core_setbit_leave_scope<int> _leaveMain(getSystemStatus(), SystemStatusAfterMain);
  __SP = sys::core_system::_getSP(); // this must be as early as possible!
  
  sys::core_system::acdk_core_static_bound = true;
#if !defined(__BORLANDC__) && !defined(__GNUG__) && !defined(__INTEL_COMPILER) 
  set_terminate(terminate_func);
#endif
  atexit(exit_func);
  int ret = -1;
  ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::InMain);
  RThreadLocal _tl = new ThreadLocal(); // force initialization
  bool thrown_exception = false;
  {
    prefilterArgs(argc, argv, envptr, addargs);
    Runtime::initSignals();
    _gSystem = Nil;
    try {
      _gSystem = new SystemImpl();
      int allargs = argc;
      int i;
      
      if (addargs != 0) {
        for (i = 0; addargs[i] != 0; i++)
          ;
        allargs += i - 1;
      }
      RStringArray args = new StringArray(argc);
      for (i = 0; i < argc; i++) {
        args[i] = SCS(argv[i]);
      }
      if (addargs != 0) {
        for (i = 0; addargs[i] != 0; i++)
          args[argc + i] = SCS(addargs[i]);
      }
      if (_gSystem->_argumentHolder != 0) {
        delete _gSystem->_argumentHolder;
        _gSystem->_argumentHolder = 0;
      }
      _gSystem->_argumentHolder = new ArgumentHolder(args);
      args = _gSystem->init(argc, argv, envptr, args);
      _saveStackBase(__SP); // must be after having initialized the mainThread!
      __SP = 0;

      
      
      _initList_t *p = &_initList;
      while(p != 0) {
        Initializer* i = p->cl;
        if (i != 0)
          i->beforeMain();
        p = p->next;
      }
      {
        sys::core_setbit_scope<int> _inMain(getSystemStatus(), SystemStatusInUsersMain);
        sys::core_setbit_leave_scope<int> _leaveUserMain(getSystemStatus(), SystemStatusAfterUsersMain);
        ret = mainfunc(args);
      }
      p = &_initList;
      while(p != 0) {
        Initializer* i = p->cl;
        if (i != 0)
          i->afterMain();
        p = p->next;
      }
      __SP = getStackBase(); // re-initializing before destroying mainThread
      _gSystem->deinit();
    } catch (RThrowable ex) {
      sys::coreout << "Caught Throwable in System::main(): "
        << ex->getMessage()->c_str() << sys::eofl;
      if (System::err != Nil)
          ex->printStackTrace();
      ret = -1;
      thrown_exception = true;
    } catch (char* msg) {
      sys::coreout << "Caught unknown char* in System::main(): \n" << msg << sys::eofl;
      sys::coreout << "Caught unknown Exception in System::main(): " << sys::eofl;
      ret = -1;
      thrown_exception = true;
    } 
    /* must not check
  catch (...) {
      sys::coreout << "Caught unknown Exception in System::main(): " << sys::eofl;
      ret = -1;
      thrown_exception = true;
    }
*/
    sys::cleanup_system_objects();
    
    
    if (thrown_exception == true && _gSystem != Nil)
      _gSystem->deinit();
    // at last, becuase deinit use ThreadLocal
    _gSystem = Nil;
    ThreadLocal::threadEnd();
    ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::AfterMain);
  }
  return ret;
}

#if defined(ACDK_OS_WIN32)
// defined in Process.cpp
RStringArray parseCommand(IN(RString) command, IN(RStringArray) _args, IN(RStringArray) _envp);

int 
System::main(ACDK_MainFunc mainfunc, HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                                           int nCmdShow, const char** addargs)
{
  
  RStringArray args = new StringArray(0);
  RStringArray env = new StringArray(0);
  RString cmdLine = SCS(lpCmdLine);
  parseCommand(cmdLine, args, env);
  ArgumentHolder tempargs(args);
  System::hasConsole = false;
  int ret = main(mainfunc, tempargs.getArgc(), tempargs.getArgv(), _environ, addargs);
  return ret;
}
#endif //defined(ACDK_OS_WIN32)

//static 
void 
System::initAsSharedLibrary()
{
  if (System::isInMain() == true)
    return;
  getSystemStatus() |= SystemStatusStartMain;

  __SP = sys::core_system::_getSP(); // this must be as early as possible!
#if !defined(__BORLANDC__) && !defined(__GNUG__) && !defined(__INTEL_COMPILER) 
  set_terminate(terminate_func);
#endif
  atexit(exit_func);
  int ret = -1;
  ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::InMain);
  Runtime::initSignals();

  _gSystem = Nil;
  try {
    _gSystem = new SystemImpl();
    RStringArray args = new StringArray(0);
    if (_gSystem->_argumentHolder != 0) {
      delete _gSystem->_argumentHolder;
      _gSystem->_argumentHolder = 0;
    }
    _gSystem->_argumentHolder = new ArgumentHolder(args);
    char* argv[1];
    argv[0] = 0;
#if defined(ACDK_OS_WIN32) && !defined(__MWERKS__) && !defined(ACDK_OS_CYGWIN32)
    args = _gSystem->init(0, argv, _environ, args);
#else
    args = _gSystem->init(0, argv, 0, args);
#endif
    _saveStackBase(__SP); // must be after having initialized the mainThread!
    __SP = 0;
    _initList_t *p = &_initList;
    while(p != 0) 
    {
      Initializer* i = p->cl;
      if (i != 0)
        i->beforeMain();
      p = p->next;
    }
    
    
  } catch (RThrowable ex) {
    sys::coreout << "Caught Throwable in System::main(): "
      << ex->getMessage()->c_str() << sys::eofl;
    if (System::err != Nil)
      ex->printStackTrace();
    ret = -1;

  } catch (char* msg) {
    sys::coreout << "Caught unknown char* in System::main(): \n" << msg << sys::eofl;
    sys::coreout << "Caught unknown Exception in System::main(): " << sys::eofl;
    ret = -1;
  } catch (...) {
    sys::coreout << "Caught unknown Exception in System::main(): " << sys::eofl;
    ret = -1;
  }
  getSystemStatus() |= SystemStatusInUsersMain | SystemStatusConfigLoaded;
}

//static 
void 
System::deInitAsSharedLibrary()
{
  if (System::isInMain() == false)
    return;
  getSystemStatus() &= ~SystemStatusInUsersMain;
  getSystemStatus() |= SystemStatusAfterUsersMain;

  _initList_t *p = &_initList;
  while(p != 0) 
  {
    Initializer* i = p->cl;
    if (i != 0)
      i->afterMain();
    p = p->next;
  }
  __SP = getStackBase(); // re-initializing before destroying mainThread
  _gSystem->deinit();
  sys::cleanup_system_objects();
  _gSystem = Nil;
  ThreadLocal::threadEnd();
  ::acdk::lang::sys::core_system::setState(::acdk::lang::sys::AfterMain);
  getSystemStatus() &= SystemStatusStartMain;
  getSystemStatus() |= SystemStatusAfterMain;

}


//static
void
System::printStackTrace(int ignoreFromTop)
{
  printStackTrace(err, ignoreFromTop + 1);
}

//static
void
System::printStackTrace(IN(acdk::io::RPrintWriter) outpw, int ignoreFromTop)
{
  ::acdk::lang::sys::BackTrace bt;
  RStackFrameArray stackFrames = bt.getStackFrames();
  printStackTrace(outpw, stackFrames, ignoreFromTop);
}

void
System_printStackFrame(IN(acdk::io::RPrintWriter) outpw, IN(RStackFrame) frame)
{
  StringBuffer sb;
  sb << "  called in ";
  if (frame->hasFileAndLine() == true)
  {
    sb << frame->getFileName() << ":" << frame->getFileLineNo();
  }
  if (frame->hasFunctionSignature() == true)
  {
    sb << ": " << frame->getFunctionSignature();
  }
  outpw->println(sb.toString());
}

//static
void
System::printStackTrace(IN(acdk::io::RPrintWriter) outpw, IN(RStackFrameArray) stackFrames, int ignoreFromTop)
{
  for (int i = ignoreFromTop; i < stackFrames.length(); i++) 
  {
    System_printStackFrame(outpw, stackFrames[i]);
  }
}


//static
int
System::identityHashCode(IN(RObject) obj)
{
  return (int)&obj;
}


//static
RString
System::getModulePath()
{
  static RString modpath;
  if (modpath != Nil)
    return modpath;
  char** p = getArgv();
  if (p == 0 || *p == 0)
    return "";
  File f(sys::core_system::fq_executable_filename(*p));
  modpath = f.getCanonicalPath();
  System::registerStaticReference(modpath);
  return modpath;
}

//static
RString
System::getModuleName(bool stripExt)
{
 
  File f(getModulePath());
  RString str = f.getName();
#ifdef ACDK_OS_WIN32
  if (stripExt == true && str->endsWith(".exe") == true)
    str = str->substr(0, str->length() - strlen(".exe"));
#endif 
  return str;
}


//static
RString
System::getModuleDir()
{
  char** p = getArgv();
  if (p == 0 || *p == 0)
    return File::getCWD();
  File f(getModulePath());
  RString str = f.getParent();
  if (str == Nil || str->length() == 0)
    return File::getCWD();
  return str;
}

RStringArray 
System::getEnvPath() 
{
  RString p = System::getEnvironment()->getProperty("PATH");
  if (p == Nil || p->length() == 0)
    return new StringArray(0);
  return acdk::util::StringTokenizer(p, acdk::io::File::pathSeparator()).allToken();
}


System::_initList_t System::_initList = {0, 0};

//ACDK_CORE_PUBLIC static 
void 
System::registerStaticReference(Object* obj) // ### @todo remove this
{
  //sys::ObjectHeap::registerStaticReference(obj);
}

void
System::registerForInitialization(Initializer* i)
{
  _initList_t *p = &_initList;
  if (p->cl == 0)  
  {
    p->cl = i;
  }
  else
  {
    while(p->next != 0) 
    {
      p = p->next;
    }
    p->next = new _initList_t;
    p->next->cl = i;
    p->next->next = 0;
  }
  if (System::isInMain() == true) // already up and running
  {
    i->beforeMain();
  }
}

//static
void
System::_saveStackBase(unsigned int SP)
{
  Thread::currentThread()->_saveStackBase(SP);
}

//static
unsigned int
System::getStackBase()
{
  if (System::isInMain() == false)
    return sys::core_system::_getSP();
  if (__SP != 0) // before or within System::main()
    return __SP;
  return Thread::getStackBase();
}

//static 
RString 
System::getErrmsg() 
{ 
  return SCS(strerror(errno)); 
}

//static 
RString 
System::getErrmsg(int errnumber) 
{ 
  return SCS(strerror(errnumber)); 
}

//static
RString
System::getLastError()
{
#ifdef ACDK_OS_WIN32
  DWORD le = GetLastError();
  LPVOID lpMsgBuf;
  FormatMessage( 
    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
    FORMAT_MESSAGE_FROM_SYSTEM | 
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    le,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    (LPTSTR) &lpMsgBuf,
    0,
    NULL 
  );
  RString ret = new String((const char*)(LPCTSTR)lpMsgBuf, NormalSST |  CCAscii);
  LocalFree( lpMsgBuf );
  return ret;
#else
  return getErrmsg();
#endif
}


//ACDK_CORE_PUBLIC static 
int 
System::getErrno() 
{ 
  return errno; 
}

//ACDK_CORE_PUBLIC static 
void 
System::resetErrno() 
{ 
  errno = 0; 
}

//ACDK_CORE_PUBLIC 
//static 
void 
System::loadLibrary(IN(RString) libname)
{
  Runtime::getRuntime()->loadLibrary(libname);
}


  /**
    return the overal limit of memory usage
  */
//ACDK_CORE_PUBLIC static 
jlong 
System::getMaxMemoryUsage()
{
  return sys::ObjectHeap::getMaxMemoryUsage();
}

//ACDK_CORE_PUBLIC static 
void 
System::setMaxMemoryUsage(jlong maxmem)
{
  sys::ObjectHeap::setMaxMemoryUsage(maxmem);
}

  /**
    return the limit of memory usage inside this thread
  */
//  ACDK_CORE_PUBLIC static 
jlong 
System::getThreadMaxMemoryUsage()
{
  return sys::ObjectHeap::getThreadMaxMemoryUsage();
}

//ACDK_CORE_PUBLIC static 
void 
System::setThreadMaxMemoryUsage(jlong maxmem)
{
  sys::ObjectHeap::setThreadMaxMemoryUsage(maxmem);
}


class System_RootObjectListenerCollector
: extends acdk::lang::Object,
  implements acdk::lang::ref::NotifyObjectEventListener
{
public:
  RObjectArray _objects;
  bool _includeStaticReferences;
  System_RootObjectListenerCollector(bool includeStaticReferences)
  : _objects(new ObjectArray(0))
  , _includeStaticReferences(includeStaticReferences)
  {
  }
  virtual void notifyBeforeConstruction(Object* obj) { }
  virtual bool notifyBeforeDestruction(Object* obj)  { return true; }
  virtual void notifyWhileDestruction(Object* obj) { }
  virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* optr, ::acdk::lang::sys::AllocatedType type, int size) 
  { 
    Object* obj = (Object*)optr;
    if (_includeStaticReferences == false)
    {
      
      if (sys::ObjectHeap::isStaticReferenceObject(obj) == true)
        return true;
      if (dynamic_cast<Class*>(obj) != 0)
        return true;
    }
    _objects->append(obj);
    return true; 
  }
};



//static 
RObjectArray 
System::getRootObjects(bool includeStaticReferences)
{
  System_RootObjectListenerCollector col(includeStaticReferences);
  sys::ObjectHeap::listObjects(&col, sys::ListRootsOnly | sys::ListObjectsRecursive);
  RObjectArray ret = col._objects;
  if (ret->length() > 0 && ret[ret->length() - 1] == ret)
    ret->remove(ret->length() - 1);
  return ret;
}
//ACDK_CORE_PUBLIC static 
RObjectArray 
System::getObjectList(int flags)
{
  System_RootObjectListenerCollector col(true);
  sys::ObjectHeap::listObjects(&col, flags);
  RObjectArray ret = col._objects;
  if (ret->length() > 0 && ret[ret->length() - 1] == ret)
    ret->remove(ret->length() - 1);
  return ret;
}

//static
int
System::getPlatformFlags()
{
  int flags = 0;
#if defined(ACDK_OS_WIN32)
  flags |= PfWin32;
#endif
#if defined(ACDK_OS_UNIX)
  flags |= PfUnix;
#endif
#if defined(ACKD_OS_LINUX)
  flags |= PfLinux;
#endif
#if defined(ACDK_OS_BSD)
  flags |= PfFreeBSD;
#endif
#if defined(ACDK_OS_SOLARIS)
  flags |= PfSolaris;
#endif
#if defined(ACDK_OS_CYGWIN32)
  flags |= PfCygwin;
#endif
#if defined(_MSC_VER)
  flags |= PfCcVc;
#endif
#if defined(__GNUG__)
  flags |= PfCcGcc;
#endif
#if defined(__BORLANDC__)
  flags |= PfCcBcc;
#endif
  flags |= Pf32Bit;
#if defined(ACDK_BIGENDIAN)
  flags |= PfBigEndian;
#endif
#if defined(ACDK_LITTLEENDIAN)
  flags |= PfLittleEndian;
#endif
  return flags;
}

} // lang
} // acdk

#if defined(ACDK_OS_CYGWIN32)

int WINAPI
acdk_core_init(HANDLE h, DWORD reason, void *foo)
{
    return 1;
}

#endif //defined(ACDK_OS_CYGWIN32)
