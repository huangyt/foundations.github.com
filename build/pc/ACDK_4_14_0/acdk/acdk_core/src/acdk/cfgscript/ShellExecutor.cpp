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



#include "ShellExecutor.h"
#include <acdk/io/CharReader.h>
#include <acdk/lang/Process.h>
#include <acdk/lang/Runtime.h>
#include <acdk/lang/System.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/File.h>
#include <acdk/locale/Encoding.h>
#include <acdk/locale/CEscapeEncoding.h>

namespace acdk {
namespace cfgscript {

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
  }
};

RString quoteCmdLine(IN(RString) cmd)
{
  return "\"" + acdk::locale::CEscapeEncoding::getCEscapeEncoding(Nil)->getEncoder()->encode(cmd) + "\"";
}

//static 
RString 
ShellExecutor::getShell(IN(RProps) props)
{
  RString sh = props->getStringVal("SHELLEXECUTE_SHELL", PropsParentRead | PropsNoWarnRead);
  if (sh != Nil && sh->length() > 1)
    return sh;
#if defined(ACDK_OS_WIN32)
  RString shellbase = "cmd";
#else
  RString shellbase = "/bin/sh";
#endif
  //LookupFileTask lf(shellbase, Executable);
  //lf.addFileName("generic", shellbase);
  //lf.execute("", props);
  //sh = lf.foundPath();
  //sh = shellbase;
  //if (sh != Nil && sh->length() > 0)
  //  return sh;
  return shellbase;
}

//static 
RString 
ShellExecutor::getShellExecuteOpt(IN(RProps) props)
{
  RString s = props->getStringVal("SHELLEXECUTE_EXECUTE_OPTION", PropsParentRead | PropsNoWarnRead);
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
ShellExecutor::execute(IN(RProps) props)
{
  
  evaluateOptions(props);
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

  RStringArray env = props->getStringArrayVal("SHELLEXECUTE_EXPORT_ENV_LIST", PropsParentRead | PropsNoWarnRead);
  RProcess process;
  if (env->length() > 0)
  {
    acdk::util::RProperties envprops = (acdk::util::RProperties)
      System::getEnvironment()->clone();
    for (int i = 0; i < env->length(); ++i)
    {
      RString key = env[i];
      RString val = props->getStringVal(key, PropsParentRead);
      ACDK_NLOG("acdk.cfgscript", Trace, "ShellExecutor: export: [" + key + "=" + val + ")");
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
    ACDK_NLOG("acdk.cfgscript", Trace, "ShellExecutor: execute: " + cmdline);
    process = Runtime::exec(cmdline, envl);
    
  }
  else
  {
    ACDK_NLOG("acdk.cfgscript", Trace, "ShellExecutor: execute: " + cmdline);
    process = Runtime::exec(cmdline);
  }
  int exits = 0;

  if ((_flags & SExecUseFileRedirect) == false)
  {
    ::acdk::io::RCharReader out = new acdk::io::ByteToCharReader(process->getOutputStream()/*, 
                                                                                             acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/
      ); // ### TODO use std encoding or better no encoding at all and let define encoding outside
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
    //Thread::sleep(300);
    exits = process->waitFor(_timeOut);
    if (exits == ProcessStillRunning)
    {
      process->destroy(); // ### TODO throw exception?
    }
    else // otherwise it will block
    {
      outreader->join();
      errreader->join();
    }
    _outs = outreader->toString();
    _errs = errreader->toString();
  
  } else {
    
    exits = process->waitFor(_timeOut);
    if (exits == ProcessStillRunning)
    {
      process->destroy();
    }
    _outs = acdk::io::ByteToCharReader(tempfileout->getReader()/*, 
                                                                 acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/).readString();
    _outs = acdk::io::ByteToCharReader(tempfileerr->getReader()/*, 
                                                                 acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder()*/).readString();
    
    tempfileout->deleteFile();
    tempfileerr->deleteFile();
  }
  if (exits == ProcessStillRunning)
    ;
  ACDK_NLOG("acdk.cfgscript", Trace, RString("ShellExecutor: execute exit with: ") + exits + ", output: " + props->getStringVal("ShellExecutor_out"));
  return exits == 0;
  }
  catch (acdk::io::RIOException ex) 
  {
    ACDK_NLOG("acdk.cfgscript", Error, "ShellExecutor: failed with IOException: commandline=[" + cmdline + "] exception=[" + ex->getMessage() + "]");
    return false;
  }
  return true;
}

//static 
void
ShellExecutor::evaluateOptions(IN(RProps) props)
{
  if (props->hasValue("SHELLEXECUTE_TIMEOUT") == true)
  {
    _timeOut = props->getIntVal("SHELLEXECUTE_TIMEOUT");
    ACDK_NLOG("acdk.cfgscript.ShellExecutor", Debug, "SHELLEXECUTE_TIMEOUT=" << _timeOut);
  }
  if (props->hasValue("SHELLEXECUTE_USEFILEREDIRECT") == true)
  {
    bool b = props->getBoolVal("SHELLEXECUTE_USEFILEREDIRECT");
    ACDK_NLOG("acdk.cfgscript.ShellExecutor", Debug, "SHELLEXECUTE_USEFILEREDIRECT=" << b);
    if (b == true)
      _flags |= SExecUseFileRedirect;
    else
      _flags &= ~SExecUseFileRedirect;
  }
  if (props->hasValue("SHELLEXECUTE_USESHELL") == true)
  {
    bool b = props->getBoolVal("SHELLEXECUTE_USESHELL");
    ACDK_NLOG("acdk.cfgscript.ShellExecutor", Debug, "SHELLEXECUTE_USESHELL=" << b);
    if (props->getBoolVal("SHELLEXECUTE_USESHELL") == true)
      _flags |= SExecUseShell;
    else
      _flags &= ~SExecUseShell;
  }
  if (props->hasValue("SHELLEXECUTE_ISSHELLSCRIPT") == true)
  {
    bool b = props->getBoolVal("SHELLEXECUTE_ISSHELLSCRIPT") ;
    ACDK_NLOG("acdk.cfgscript.ShellExecutor", Debug, "SHELLEXECUTE_ISSHELLSCRIPT=" << b);
    if (b == true)
      _flags |= SExecIsScript;
    else
      _flags &= ~SExecIsScript;
  }
  
}

} // namespace cfgscript
} // namespace acdk 
  
