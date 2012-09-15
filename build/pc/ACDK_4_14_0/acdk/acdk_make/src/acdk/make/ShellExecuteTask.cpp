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


#include "ShellExecuteTask.h"
#include "LookupFileTask.h"
#include <acdk/lang/Process.h>
#include <acdk/lang/Runtime.h>
#include <acdk/lang/System.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/io/NullWriter.h>
#include <acdk/locale/Encoding.h>
#include <acdk/locale/CEscapeEncoding.h>


namespace acdk {
namespace make {

using namespace acdk::cfgscript;

ACDK_DECL_CLASS(ProcessOutReaderThread);

class ProcessOutReaderThread
: extends ::acdk::lang::Thread
{
public:
  StringBuffer _sb;
  ::acdk::io::RCharReader _in;
  /**
    Where to write
  */
  ::acdk::io::RCharWriter _out;
  ProcessOutReaderThread(IN(::acdk::io::RCharReader) in, IN(::acdk::io::RCharWriter) out = Nil)
  : _in(in)
  , _out(out)
  {
    if (_out == Nil)
    {
      //_out = &System::out;
    }
  }
  RString toString() { return _sb.toString(); }
  void run()
  {
    try  {
      int buf;
      while ((buf = _in->readChar()) != -1)  
      {
        _sb.append((uc2char)buf);
        if (_out != Nil)
        {
          _out->writeChar((ucchar)buf);
          if (buf == '\n')
            _out->flush();
        }
      } 
    } catch (::acdk::io::RIOException ex) {
      //System::out->println("PORTH ex: " + ex->getMessage());
    }
    //std::cout << "PORTH exit" << std::endl;
  }
};

RString quoteCmdLine(IN(RString) cmd)
{
  return "\"" + acdk::locale::CEscapeEncoding::getCEscapeEncoding(Nil)->getEncoder()->encode(cmd) + "\"";
}

//static 
RString 
ShellExecuteTask::getShell(IN(RProps) props)
{
  RString sh = props->getStringVal("SHELL", PropsParentRead | PropsNoWarnRead);
  if (sh != Nil && sh->length() > 1)
    return sh;
#if defined(ACDK_OS_WIN32)
  RString shellbase = "cmd";
#else
  RString shellbase = "sh";
#endif
  LookupFileTask lf(shellbase, Executable);
  lf.addFileName("generic", shellbase);
  lf.execute("", props);
  sh = lf.foundPath();
  if (sh != Nil && sh->length() > 0)
    return sh;
  return shellbase;
}
//static 
RString 
ShellExecuteTask::getShellExecuteOpt(IN(RProps) props)
{
  RString s = props->getStringVal("SHELL_EXECUTE_OPTION", PropsParentRead | PropsNoWarnRead);
  if (s != Nil && s->length() > 0)
    return s;
#if defined(ACDK_OS_WIN32)
  return "/C";
#else
  return "-c";
#endif
}

//virtual 
bool 
ShellExecuteTask::execute(IN(RString) exec, IN(RProps) props)
{
  
  RString cmdline;
  ::acdk::io::RFile tempfileout;
  ::acdk::io::RFile tempfileerr;
  if ((_flags & SExecUseShell) || (_flags & SExecUseFileRedirect))
  {
    RString shell = getShell(props) + " " + getShellExecuteOpt(props);
    RString quotedcmdl = quoteCmdLine(_cmdline);

    if (_flags & SExecUseFileRedirect)
    {
      tempfileout = ::acdk::io::File::createTempFile("acdk_system_out", Nil);
      tempfileerr = ::acdk::io::File::createTempFile("acdk_system_err", Nil);
      RString redir = " 2>" + tempfileerr->getCanonicalPath() + " 1>" + tempfileout->getCanonicalPath();
      cmdline = shell + " " + quotedcmdl + redir;
    } else {
      cmdline = shell + " " + quotedcmdl;
    }
  }
  else if (_flags & SExecIsScript)
  {
    cmdline = getShell(props) + " " + _cmdline;
  }
  else
  {
    cmdline = _cmdline;
  }
  if (_flags & SExecEvalBeforeExec)
  {
    cmdline = props->eval(cmdline, PropsParentRead);
  }
  if (_env != Nil && (_flags & SExecUseCleanEnv) == false)
  {
    
  }
  try {

  RStringArray env = props->getStringArrayVal("SHELL_EXPORT_ENV_LIST", PropsParentRead | PropsNoWarnRead);
  RProcess process;
  if (env->length() > 0)
  {
    acdk::util::RProperties envprops = (acdk::util::RProperties)
      System::getEnvironment()->clone();
    for (int i = 0; i < env->length(); ++i)
    {
      RString key = env[i];
      RString val = props->getStringVal(key, PropsParentRead);
      ACDK_NLOG("acdk.make", Trace, "ShellExecuteTask: export: [" + key + "=" + val + ")");
      envprops->setProperty(key, val);
    }
    acdk::util::RIterator it = envprops->propertyNames();
    RStringArray envl = new StringArray(0);
    while (it->hasNext() == true)
    {
      RString key = (RString)it->next();
      RString val = envprops->getProperty(key);
      envl->append(key + "=" + val);
    }
    ACDK_NLOG("acdk.make", Trace, "ShellExecuteTask: execute: " + cmdline);
    process = Runtime::exec(cmdline, envl);
    
  }
  else
  {
    ACDK_NLOG("acdk.make", Trace, "ShellExecuteTask: execute: " + cmdline);
    process = Runtime::exec(cmdline);
  }
  int exits = 0;
  if ((_flags & SExecUseFileRedirect) == false)
  {
    ::acdk::io::RCharReader out = new acdk::io::ByteToCharReader(process->getOutputStream()/*, 
                                                                                             acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/
      ); // ### TODO use std encoding
    ::acdk::io::RCharReader err = new acdk::io::ByteToCharReader(process->getErrorStream()/*, 
                                                                                            acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/
      );

    if (_outWriter == Nil)
    {
      if (_flags & SExecNoStdOut)
        _outWriter = Nil;//new acdk::io::NullWriter();
      else
        _outWriter = &System::out;
    }
    if (_errWriter == Nil)
    {
      if (_flags & SExecNoErrOut)
        _errWriter = Nil;//new acdk::io::NullWriter();
      else
        _errWriter = &System::err;
    }
    RProcessOutReaderThread outreader = new ProcessOutReaderThread(out, _outWriter);
    RProcessOutReaderThread errreader = new ProcessOutReaderThread(err, _errWriter);
    outreader->start();
    errreader->start();
    Thread::sleep(300);
    exits = process->waitFor();
    outreader->join();
    errreader->join();
    _outs = outreader->toString();
    _errs = errreader->toString();
    props->setStringVal("ShellExecuteTask_out", _outs, PropsNoFlags);
    props->setStringVal("ShellExecuteTask_err", _errs, PropsNoFlags);

    /*
    if (outs->length() > 0)
      System::out->println(RString("OUT: ") +outs);
    if (errs->length() > 0)
      System::out->println(RString("ERR: ") + errs);
      */
  } else {
    
    exits = process->waitFor();
    _outs = acdk::io::ByteToCharReader(tempfileout->getReader()/*, 
                                                                 acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/).readString();
    _outs = acdk::io::ByteToCharReader(tempfileerr->getReader()/*, 
                                                                 acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/).readString();
    props->setStringVal("ShellExecuteTask_out", _outs, PropsNoFlags);
    props->setStringVal("ShellExecuteTask_err", _errs, PropsNoFlags);
    /*
    if (outs->length() > 0)
      System::out->println(RString("OUT: ") + outs);
    if (errs->length() > 0)
      System::out->println(RString("ERR: ") + errs);
    */
    tempfileout->deleteFile();
    tempfileerr->deleteFile();
  }
  ACDK_NLOG("acdk.make", Trace, RString("ShellExecuteTask: execute exit with: ") + exits + ", output: " + props->getStringVal("ShellExecuteTask_out"));
  return exits == 0;
  }
  catch (acdk::io::RIOException ex) 
  {
    ACDK_NLOG("acdk.make", Error, "ShellExecuteTask: failed with IOException: commandline=[" + cmdline + "] exception=[" + ex->getMessage() + "]");
    return false;
  }
  return true;
}



} // namespace make
} // namespace acdk



