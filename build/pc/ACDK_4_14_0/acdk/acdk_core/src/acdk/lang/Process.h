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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Process.h,v 1.24 2005/04/09 19:26:49 kommer Exp $

#ifndef acdk_lang_Process_h
#define acdk_lang_Process_h

#include <acdk.h>


#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileDescriptor.h>

#ifdef ACDK_OS_WIN32
#  include <winbase.h>
#endif

namespace acdk {
namespace lang {

//using namespace io;

ACDK_DECL_CLASS(Process_PipeReader);
/** 
  Helper class to reading process output
  API: ACDK internal<br>
  @author Roger Kommer, Wolfgang Jung, Juergen Baumann
  @version $Revision: 1.24 $
  @date $Date: 2005/04/09 19:26:49 $
*/  
foreign
class ACDK_CORE_PUBLIC Process_PipeReader
: extends ::acdk::io::FileReader
{
public:
  Process_PipeReader( IN(::acdk::io::RFileDescriptor) fd, bool dupl = false);
  
  foreign int available();
  foreign bool ready() { return (available() > 0)? true : false; }
  foreign int read();
  foreign int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign int read(byte* buffer, int offset, int len);
  foreign RString readLine();
};


enum ProcessStatus
{
  ProcessStillRunning     = 0x1000,
  ProcessFinished         = 0x2000,
  ProcessAlreadyDestroyed = 0x4000,
  ProcessNotStarted       = 0x8000,
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, ProcessStatus);

/** 
  Extern process/executable.
  @see acdk::cfgscript::ShellExecutor for a more easy to use
       class for launching external processes.

  @author Roger Kommer, Wolfgang Jung, Juergen Baumann
  @version $Revision: 1.24 $
  @date $Date: 2005/04/09 19:26:49 $
*/  

class ACDK_CORE_PUBLIC Process
: extends Object
{
  ACDK_WITH_METAINFO(Process)
protected:
  Process(IN(RString) command, IN(RString) workdir = Nil);
  Process(IN(RStringArray) cmdarray, IN(RString) workdir = Nil);
  Process(IN(RStringArray) cmdarray, IN(RStringArray) envp, IN(RString) workdir = Nil);
  Process(IN(RString) command, IN(RStringArray) envp, IN(RString) workdir = Nil);
  friend class Runtime;
  int _state;
public:
  foreign virtual ~Process();
  /*
    try to kill process
    @return if is ProcessStillRunning process still is running
  */
  void destroy();
  /**
    return the command line executed
  */
  RString getCommandLine();
  /**
    return the exit value
  */
  int exitValue() { return _exitCode; }
  /**
    Process are binary Reader/Writer
    The encoding of character depends of the started application
  */
  acdk::io::RReader getOutputReader() { return getOutputStream(); }
  /**
    @see getErrorReader()
  */
  acdk::io::RReader getErrorReader() { return getErrorStream(); }
  /**
    @see getErrorReader()
  */
  acdk::io::RWriter getInputWriter() { return getInputStream(); }
  
  /** alias to getErrorReader */
  acdk::io::RReader getErrorStream();
  /** alias to getInputWriter */
  acdk::io::RWriter getInputStream();
  /** alias to getOutputReader */
  acdk::io::RReader getOutputStream();
  /**
    wait for ending of the process
    @return if process is still running ProcessStillRunning 
            if process already was destroyed ProcessAlreadyDestroyed
            
  */
  int waitFor(int timeOutMs = -1);
  /**
    @return true if process is still running
  */
  bool isRunning();
  /**
    stop the process
  */
  void suspend();
  /**
    resume the process
  */
  void resume();
  /**
    @return the current os process id
  */
  static int getProcessId();
private:
  virtual void _run();


  RString _command;
  RStringArray _args;
  RStringArray _envp;
  RString _workDir;
  RProcess_PipeReader _stderrReader;
  RProcess_PipeReader _stdoutReader;
  acdk::io::RFileWriter _stdinWriter;
  ProcessStatus _status;
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)

  _STARTUPINFOW _startupInfo;
  PROCESS_INFORMATION  _processInfo;
#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  pid_t _processId;
#else // ACDK_OS_*
#error currently no process creation available for this platform/OS
#endif

  int _exitCode;
};

} // lang
} // acdk

#endif //acdk_lang_Process_h

