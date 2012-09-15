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

#ifndef acdk_cfgscript_ShellExecutor_h
#define acdk_cfgscript_ShellExecutor_h


#include "Props.h"
#include <acdk/io/CharWriter.h>

namespace acdk {
namespace cfgscript {


enum ShellExecuteFlags
{
  /**
    do not execute cmd directly but use a shell, like sh or cmd
    Note: 
  */
  SExecUseShell          = 0x01,
  /**
    use tempory files for output capture
  */
  SExecUseFileRedirect   = 0x02,
  /**
    don't use the system environment, but 
    a clean empty enviromnet
  */
  SExecUseCleanEnv       = 0x04,
  /**
    the command line is a script (.cmd or .sh)
    and not an executable
    implies SExecUseShell
  */
  SExecIsScript          = 0x08,
  /**
    the command line will be evaluated by acdk::cfgscript::Script
    before execute it
  */
  SExecEvalBeforeExec    = 0x10,
  /**
    don't print std out to writer
  */
  SExecNoStdOut          = 0x20,
  /**
    don't print std err to writer
  */
  SExecNoErrOut          = 0x40,

  SExecNoOut          = SExecNoStdOut | SExecNoErrOut
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, ShellExecuteFlags);



ACDK_DECL_CLASS(ShellExecutor);

/**
  General class to execute external programs.

*/
class ACDK_CFGSCRIPT_LIB_PUBLIC ShellExecutor
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ShellExecutor)
private:
  RString _cmdline;
  int _flags;
  RStringArray _env;
  RString _workDir;
  RString _outs;
  RString _errs;
  ::acdk::io::RCharWriter _outWriter;
  ::acdk::io::RCharWriter _errWriter;
  /**
    timeout in ms waiting for process
    by default -1, wait forever
  */
  int _timeOut;
public:
  /**
    @param the command line for the external programm to execute
    @param flags a combination of ShellExecuteFlags
    @param env Environment to use
  */
  ShellExecutor(IN(RString) cmdline, int flags, IN(RStringArray) env = Nil)
  : _cmdline(cmdline)
  , _flags(flags)
  , _env(env)
  , _outs("")
  , _errs("")
  , _timeOut(-1)
  {
  }
  
  void setCommandLine(IN(RString) cmdline) { _cmdline = cmdline; }
  /**
    set the working directory where to execute the program
    If it is not set uses the working directory of the parent process
  */
  void setWorkingDir(IN(RString) wdir) { _workDir = wdir; }
  /**
    run the external programm
  */
  virtual bool execute(IN(RProps) props);
  /**
    after returning from execute() this method 
    returns the standard output of the executable
  */
  RString getOutString() { return _outs; }
  /**
    after returning from execute() this method 
    returns the error output of the executable
  */
  RString getErrString() { return _errs; }
  /**
    Set the writer, where to write the standard output
    By default the output will be collected in a string
    which can be retrived via getOutString()
  */
  void setOutWriter(IN(::acdk::io::RCharWriter) outWriter)
  {
    _outWriter = outWriter;
  }
  /**
    Set the writer, where to write the standard output
    By default the output will be collected in a string
    which can be retrived via getOutString()
  */
  void setErrWriter(IN(::acdk::io::RCharWriter) errWriter)
  {
    _errWriter = errWriter;
  }
  /**
    see _timeOut
  */
  int getTimeout() { return _timeOut; }
  /**
    see _timeOut
  */
  void setTimeOut(int timeOut) { _timeOut = timeOut; }

  static RString getShell(IN(RProps) props);
  static RString getShellExecuteOpt(IN(RProps) props);
  void evaluateOptions(IN(RProps) props);

};

} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ShellExecutor_h
