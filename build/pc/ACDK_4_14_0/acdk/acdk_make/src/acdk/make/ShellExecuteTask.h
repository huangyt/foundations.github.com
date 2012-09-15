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

#ifndef acdk_make_ShellExecuteTask_h
#define acdk_make_ShellExecuteTask_h

#include "AbstractTask.h"



namespace acdk {
namespace make {

enum ShellExecuteFlags
{
  SExecUseShell          = 0x01,
  SExecUseFileRedirect   = 0x02,
  SExecUseCleanEnv       = 0x04,
  SExecIsScript          = 0x08,
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
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, ShellExecuteFlags);

ACDK_DECL_CLASS(ShellExecuteTask);

/**
  Execute an external process.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC ShellExecuteTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(ShellExecuteTask)
public:
  RString _cmdline;
  int _flags;
  RStringArray _env;
  RString _workDir;
  RString _outs;
  RString _errs;
  ::acdk::io::RCharWriter _outWriter;
  ::acdk::io::RCharWriter _errWriter;
  ShellExecuteTask(IN(RString) name, IN(RString) descr, IN(RString) cmdline, int flags, IN(RStringArray) env = Nil)
  : AbstractTask(name, name, descr)
  , _cmdline(cmdline)
  , _flags(flags)
  , _env(env)
  , _outs("")
  , _errs("")
  {
  }
  static RString getShell(IN(RProps) props);
  static RString getShellExecuteOpt(IN(RProps) props);
  void setCommandLine(IN(RString) cmdline) { _cmdline = cmdline; }
  void setWorkingDir(IN(RString) wdir) { _workDir = wdir; }
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  RString getOutString() { return _outs; }
  RString getErrString() { return _errs; }
  void setOutWriter(IN(::acdk::io::RCharWriter) outWriter)
  {
    _outWriter = outWriter;
  }
  void setErrWriter(IN(::acdk::io::RCharWriter) errWriter)
  {
    _errWriter = errWriter;
  }
};



} // namespace make
} // namespace acdk


#endif //acdk_make_ShellExecuteTask_h
