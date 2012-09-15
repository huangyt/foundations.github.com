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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Process.cpp,v 1.53 2005/04/25 23:03:34 kommer Exp $



#include <acdk.h>
#include "Thread.h"  // needed for Thread::sleep()
#include "Process.h"
#include "System.h"  // needed for Process::_run()
#include "Integer.h"
#include "sys/core_tick.h"
#include "../util/SysDate.h"

#include <stdlib.h>

#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_CYGWIN32) || defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_BSD) || defined(ACDK_OS_DARWIN)
# include <sys/types.h>
# include <sys/wait.h>
# include <signal.h>
# include <errno.h>
# include <unistd.h>
#endif

#include "ObjectArrayImpl.h"
#include "IllegalThreadStateException.h"

#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileDescriptor.h>
#include <acdk/io/IOException.h>
#include <acdk/util/StringTokenizer.h>
#if defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
#include <acdk/util/StringTokenizer.h>
#include <acdk/util/NoSuchElementException.h>
#include <acdk/io/File.h>
#include <acdk/io/FileReader.h>
#endif // defined(ACDK_OS_UNIX)
#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/StringReader.h>

#include "SystemIntern.h"

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#include <tlhelp32.h>

#endif 

namespace acdk {
namespace lang {

/*
#if defined(ACDK_MINGW)

int _open_osfhandle ( long osfhandle, int flags )
{
  return -1;
}
long _get_osfhandle( int filehandle )
{
  return 0;
}

#endif //defined(ACDK_MINGW)
*/

//#define LOCAL_DEBUG

#ifdef LOCAL_DEBUG
#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr)
#endif

Process_PipeReader::Process_PipeReader(IN(::acdk::io::RFileDescriptor) fd, bool dupl)
: ::acdk::io::FileReader(fd, dupl)
{
}

USING_CLASS(acdk::io::, FileDescriptor);
USING_CLASS(acdk::io::, IOException);
USING_CLASS(acdk::io::, Reader);
USING_CLASS(acdk::io::, Writer);
USING_CLASS(acdk::io::, FileReader);
USING_CLASS(acdk::io::, FileWriter);

//using namespace ::acdk::util;

String::iterator consumeWs(String::iterator it, String::iterator end)
{
  while (it < end)
  {
    if (*it != ' ' && *it != '\t')
    {
      --it;
      return it;
    }
    ++it;
  }
  return it;
}

RStringArray
parseCommand(IN(RString) command, IN(RStringArray) _args, IN(RStringArray) _envp)
{
  acdk::io::StringReader cin(command);
  acdk::io::StreamTokenizer tin(&cin);
  tin.readCxxIdentifier(false);
  tin.readOperator(false);
  tin.readNumberAsString(true);
  tin.wantWhiteSpace(true);
  tin.parseCCComments(false);
  int tk;
  StringBuffer sb;
  while ((tk = tin.nextToken()) != acdk::io::StreamTokenizer::TT_EOF)
  {
    if (tk == acdk::io::StreamTokenizer::TT_WS)
    {
      _args->append(sb.toString());
      sb.reset();
    }
    else
    {
      sb.append(tin.sval);
    }
  }
  if (sb.length() > 0)
    _args->append(sb.toString());
  return _args;
}

/* old
RStringArray _parseCommand(IN(RString) command, bool expand,
                          IN(RStringArray) _args, IN(RStringArray) _envp)
{
  if (expand == true)
  {
    sys::coreout << "parseCommand expand = true" << sys::eofl;
    int argind = 0;
    int envind = 0;
    int n;
    bool quote = false;
    bool escape = false;
    bool wsp = false;
    bool isenv = false;
    bool envable = true;
    int start = 0;
    StringBuffer sb;
    for (n = 0; n < command->length(); n++)
    {
      char c = command->charAt(n);
      if (escape == true)
      {
        escape = false;
        sb.append(c);
        continue;
      }
      if ((quote == true) && (c == '"')) {
        sb.append(c);
        quote = false;
        continue;
      }
      switch (c) {
        case '"':
          envable = false;
          quote = true;
          break;
        case '\\':
          envable = false;
          escape = true;
          break;
        case ' ':
        case '\t':
          if (sb.length() == 0)
            break;
          if (isenv == true) {
            _envp->resize(envind + 1);
            _envp->set(envind++, sb.toString());
            isenv = false;
            envable = true;
          } else {
            _args->resize(argind + 1);
            _args->set(argind++, sb.toString());
            envable = false;
          }
          sb.reset();
          break;
        case '=':
          if (envable == true) {
            isenv = true;
          }
          // fall through
        default:
          sb.append(c);
          break;
      }
    }
    _args->resize(argind + 1);
    _args->set(argind++, sb.toString());
    _args->resize(argind); // needed, because ensureCapacity() may have expanded it too much.
    _envp->resize(envind);
    sb = Nil;
  }
  else
{
    bool inDoubleQuote = false;
    bool inSingleQuote = false;
    bool nextQuoted = false;
    StringBuffer sb;
    sys::coreout << command->c_str() << sys::eofl;
    for (String::iterator it = command->begin(); it < command->end(); ++it)
    {
      if (*it == '\\')
      {
        nextQuoted = true;
        //sb.append(*it);
        continue;
      }
      else if (*it == '"')
      {
        if (nextQuoted == true)
        {
          sb.append(*it);
        }
        else if (inSingleQuote == true)
        {
          sb.append(*it);
        }
        else if (inDoubleQuote == true)
        {
          _args->append(sb.toString());
          sb.reset();
          inDoubleQuote = false;
        } else
          inDoubleQuote = true;
      }
      else if (*it == '\'')
      {
        if (nextQuoted == true)
        {
          sb.append(*it);
        }
        else if (inDoubleQuote == true)
        {
          sb.append(*it);
        }
        else if (inSingleQuote == true)
        {
          _args->append(sb.toString());
          sb.reset();
          inSingleQuote = false;
        } else
          inSingleQuote = true;
      }
      else if (*it == ' ' || *it == '\t')
      {
        if (inSingleQuote == true || inDoubleQuote == true)
        {
          sb.append(*it);
        }
        else
        {
          _args->append(sb.toString());
          sb.reset();
          it = consumeWs(it, command->end());
        }
      }
      else
      {
        sb.append(*it);
      }
      nextQuoted = false;
    }
    if (sb.length() > 0)
      _args->append(sb.toString());
  }
  return _args;
}
*/

Process::Process(IN(RString) command, IN(RString) workdir)
: _command(command)
, _workDir(workdir)
, _status(ProcessNotStarted)
{
  _args = new StringArray(0);
  _envp = new StringArray(0);
  parseCommand(command, _args, _envp);
    _run();
}

Process::Process(IN(RString) command, IN(RStringArray) envp, IN(RString) workdir)
: _command(command)
, _workDir(workdir)
, _status(ProcessNotStarted)
{
  _args = new StringArray(0);
  _envp = envp;
  parseCommand(command, _args, _envp);
  _run();
}

Process::Process(IN(RStringArray) cmdarray, IN(RStringArray) envp, IN(RString) workdir)
: _workDir(workdir)
, _status(ProcessNotStarted)
{
  _args = cmdarray;
  _envp = envp;

  _run();
}

Process::Process(IN(RStringArray) cmdarray, IN(RString) workdir)
: _workDir(workdir)
, _status(ProcessNotStarted)
{
  _args = cmdarray;
  _envp = new StringArray(0);
  _run();
}

RString
Process::getCommandLine()
{
  if (_command != Nil)
    return _command;
  StringBuffer sb;
  for (int i = 0; i < _args->length(); ++i)
  {
    if (i > 0)
      sb << " ";
    RString s = _args[i];
    if (s->indexOf(' ') != -1 ||
        s->indexOf('\t') != -1 ||
        s->indexOf('\"') != -1 ||
        s->indexOf('\'') != -1)
    {
      if (s->indexOf('\"') != -1)
        sb << "\"" << s << "\"";
      else
        sb << "\'" << s << "\'";
    }
    else
      sb << s;
  }
  _command = sb.toString();
  return _command;
}

//virtual
void
Process::destroy()
{
  DOUT("Process::destroy()");
 if (_status == ProcessAlreadyDestroyed)
   return;

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  TerminateProcess(_processInfo.hProcess, ERROR_PROCESS_ABORTED);
#else
  
  int n;
  kill(_processId, SIGTERM);  // maybe we should sent SIGKILL immediately
  for (n = 0; n < 10; n++) 
  {
    if (isRunning() == false)
      return;
    Thread::sleep(20);
  }
  kill(_processId, SIGKILL);  // note: SIGINT is catchable!!
  DOUT("Process::destroyed");
#endif
  _status = ProcessAlreadyDestroyed;
}

Process::~Process() 
{
  DOUT("Process::~Process()");
  if (_status == ProcessStillRunning)
    waitFor();

  _stdinWriter = Nil;
  _stdoutReader = Nil;
  _stderrReader = Nil;

  _args = Nil;

  if (_envp) {
    _envp = Nil;
  }
}

int
Process_PipeReader::available()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  DWORD avail;
  if (PeekNamedPipe((HANDLE)(_get_osfhandle(getFD()->c_fd())), NULL, NULL, NULL, &avail, NULL) == FALSE)
  {
    DOUT("PeekNamedPipe failed: " << System::getLastError());
    return 0;
  }
  return avail;

#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  struct stat st;

  if ((fstat(getFD()->c_fd(), &st)) < 0)
  {
    DOUT("Process_PipeReader(): fstat failed");
    return 0;
  }
  return st.st_size;
#else // ACDK_OS_*

#error pipes currently not supported on this platform/OS.
#endif // ACDK_OS_*
}

int
Process_PipeReader::read()
{
  return FileReader::read();
}

RString
Process_PipeReader::readLine()
{
  return FileReader::readLine();
}


int
Process_PipeReader::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (len == -1)
    len = buffer->length() - offset;
  //if (available() >= len)
  return FileReader::read(buffer, offset, len);
  //return -1;
}

int
Process_PipeReader::read(byte* buffer, int offset, int len)
{
  return FileReader::read(buffer, offset, len);
}



void
Process::_run()
{

  RFileDescriptor iFD, oFD, eFD;
  _exitCode = -1;

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)

  HANDLE inh[2], outh[2], errh[2], tmph;
  int fd;
  bool retval;
  StringBuffer cbuf("");
  ZeroMemory(&_startupInfo, sizeof(_STARTUPINFOW));
  _startupInfo.cb = sizeof(_STARTUPINFOW);
  _startupInfo.lpReserved = NULL;
  _startupInfo.lpTitle = NULL;
  _startupInfo.lpDesktop = NULL;
  _startupInfo.dwFlags = STARTF_USESTDHANDLES;
  _startupInfo.lpReserved2 = NULL;
  _startupInfo.cbReserved2 = 0;

  SECURITY_ATTRIBUTES sa;
  ZeroMemory(&sa, sizeof(SECURITY_ATTRIBUTES));
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = FALSE;


  // Create Stdin
  retval = CreatePipe(&(inh[0]), &(inh[1]), &sa, 0);
  if (retval == 0)
    THROW1(IOException, "CreatePipe");
  retval = DuplicateHandle(GetCurrentProcess(), inh[0], GetCurrentProcess(), &tmph, 0,
                           TRUE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE);
  if (retval == 0)
    THROW1(IOException, "DuplicatePipe");

  iFD = new FileDescriptor(_open_osfhandle((long)(inh[1]), 0), O_WRONLY);
  _startupInfo.hStdInput = tmph;

  // Create Stdout
  retval = CreatePipe(&(outh[0]), &(outh[1]), &sa, 0);
  if (retval == 0)
    THROW1(IOException, "CreatePipe");
  retval = DuplicateHandle(GetCurrentProcess(), outh[1], GetCurrentProcess(), &tmph, 0,
                                    TRUE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE);
  if (retval == 0)
    THROW1(IOException, "DuplicatePipe");
  oFD = new FileDescriptor(_open_osfhandle((long)(outh[0]), 0), O_RDONLY);
  _startupInfo.hStdOutput = tmph;

  // Create Stderr
  retval = CreatePipe(&(errh[0]), &(errh[1]), &sa, 0);
  if (retval == 0)
    THROW1(IOException, "CreatePipe");
  retval = DuplicateHandle(GetCurrentProcess(), errh[1], GetCurrentProcess(), &tmph, 0,
                          TRUE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE);
  if (retval == 0)
    THROW1(IOException, "DuplicatePipe");

  eFD = new FileDescriptor(_open_osfhandle((long)(errh[0]), 0), O_RDONLY);
  _startupInfo.hStdError = tmph;
  /*
  char *wsp, *enviro = 0;
  int i = 0;
  while(_argv[i] != 0)  {
    if ((wsp = strpbrk(const_cast<char *>(_argv[i]), " \t\r\n")) != NULL)
      cbuf.append('"');
    cbuf.append(_argv[i]);
    if (wsp != NULL)
      cbuf.append('"');
    cbuf.append(" ");
    i++;
  }
  */

  // Build up Environmental Block
  UcEnvBlockHolder envblock(_envp);

  const native_char* workdirptr = 0;
  if (_workDir != Nil)
  {
    _workDir = _workDir->convertToNative();
    workdirptr = _workDir->native_c_str();
  }
  /*
  char *cmd = const_cast<char*>(cbuf.c_str());
  int cmdlength = strlen(cmd);
  */
  RString cmdline = getCommandLine();
  cmdline = cmdline->convertToNative();

  native_char* cmdptr = const_cast<native_char*>(cmdline->native_c_str());
  retval = CreateProcessW(NULL, reinterpret_cast<LPWSTR>(cmdptr), NULL, NULL, true, 0/*DETACHED_PROCESS*/ | CREATE_UNICODE_ENVIRONMENT , envblock.block, 
						  (LPCWSTR)workdirptr, &_startupInfo, &_processInfo);
  if (retval == 0)
  {
    THROW1(IOException, RString("Cannot create process: ") + getCommandLine());
  }
  CloseHandle(_startupInfo.hStdOutput);
  CloseHandle(_startupInfo.hStdError);

#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)

  int n, cnt;
  ArgumentHolder tempargs(_args);
  ArgumentHolder tempenvp(_envp);
  char** _argv = tempargs.getArgv();
  char** _env = (tempenvp.getArgc() > 0)? tempenvp.getArgv() : 0;

  int infd[2], outfd[2], errfd[2], tmpfd;
  int fakefd[2];
  int retval;
  RString msg;
  RStringBuffer path = new StringBuffer;
  if (_args[0]->indexOf('/') == -1) {
    ::acdk::util::RStringTokenizer st = new ::acdk::util::StringTokenizer(System::getProperty("PATH"), RString(":"));
    RString pe;
    ::acdk::io::RFile tf;
    while (st->hasNext() == true)
    {
      if ((pe = st->nextToken()) == Nil)
      {
        // shouldn't happen after hasNext()
        RString msg = RString("error while parsing \"") + System::getProperty("PATH") + "\"";
        THROW1(IOException, msg);
      }
      path->set(pe);
      if ((pe->length() > 0) && (pe->charAt(pe->length() - 1) != '/'))
        path->append("/");
      path->append(_args[0]);
      tf = new ::acdk::io::File(path->toString());
      if (tf->exists())
        break;
      tf = Nil;
      pe = Nil;
    }
    tf = Nil;
    st = Nil;
    if (pe == Nil) {
      path->set(_args[0]);
    }
    pe = Nil;
  } else {
    path->set(_args[0]);
  }

  retval = pipe(infd);
  if (retval < 0)
    THROW1(IOException, "pipe fd0");
  retval = pipe(outfd);
  if (retval < 0)
    THROW1(IOException, "pipe fd1");
  retval = pipe(errfd);
  if (retval < 0)
    THROW1(IOException, "pipe fd2");
  retval = pipe(fakefd);
  if (retval < 0)
    THROW1(IOException, "pipe fd3");


  // WARNING: we should never throw an exception from client or the results are unexpectable

  switch (_processId = fork()) {
    case -1:  // fork failed.
      THROW1(IOException, "create new process failed");
      break;
    case 0: // am child
      ::close(infd[1]);
      ::close(outfd[0]);
      ::close(errfd[0]);
      ::close(fakefd[0]);
      for (int n = 0; n < 32; n++)
        signal(n, SIG_DFL);
      if ((::dup2(infd[0], 0)) != 0)
      {
        msg = RString("dup2 of in-pipe failed: errno=") + Integer::toString(errno) + "\n";
        write(errfd[1], msg->c_str(), msg->length());
        break;
      }
      if ((::dup2(outfd[1], 1)) != 1) {
        msg = RString("dup2 of out-pipe failed: errno=") + Integer::toString(errno) + "\n";
        write(errfd[1], msg->c_str(), msg->length());
        break;
      }
      if ((::dup2(errfd[1], 2)) != 2) {
        msg = RString("dup2 of err-pipe failed: errno=") + Integer::toString(errno) + "\n";
        write(errfd[1], msg->c_str(), msg->length());
        break;
      }
      ::close(infd[0]);
      ::close(outfd[1]);
      ::close(errfd[1]);
      if ((::fcntl(fakefd[1], F_SETFD, FD_CLOEXEC) < 0) || (::fcntl(fakefd[1], F_GETFD) != FD_CLOEXEC))
        THROW1(IOException, "closeonexec failed.");


       if (_workDir != Nil)
          chdir(_workDir->c_str());

        // execv() is declared as execv(const char *, char * const *), so we need const_cast<>()
        // note: exec only returns, if it fails.
       /*
       sys::coreout << "EXEC: " << path->c_str() << sys::eofl;
       for (const char** ptr = (const char**)_argv; *ptr != 0; ++ptr)
          sys::coreout << "arg: " <<  *ptr << sys::eofl;
       */
       if (_env)
      {
        //sys::coreout << "execve" << sys::eofl;
        execve(path->c_str(), const_cast<char * const *>(_argv), const_cast<char * const *>(_env));
      } else {
        /*
        for (const char** ptr = (const char**)environ; *ptr != 0; ++ptr)
          sys::coreout << "env: " <<  *ptr << sys::eofl;
        for (const char** ptr = (const char**)_argv; *ptr != 0; ++ptr)
          sys::coreout << "arg: " <<  *ptr << sys::eofl;
        */
        execv(path->c_str(), const_cast<char * const *>(_argv));
      }
      ::close(fakefd[1]);
      ::fcntl(2, F_SETFL, O_NONBLOCK);
      msg = RString("execv of \"") + path->toString() + "\" failed: errno=" + Integer::toString(errno) + "\n";
      ::write(2, msg->c_str(), msg->length());
      break;
    default:  // am parent
      DOUT("start process pid: " << _processId);
      ::close(infd[0]);
      ::close(outfd[1]);
      ::close(errfd[1]);
      ::close(fakefd[1]);
      iFD = new FileDescriptor(infd[1], O_WRONLY);
      oFD = new FileDescriptor(outfd[0], O_RDONLY);
      eFD = new FileDescriptor(errfd[0], O_RDONLY);
      break;
  }
  if (_processId == 0) { // only meaningful for failed client, which should not reach this point.
    ::close(infd[0]);
    ::close(outfd[0]);
    ::close(errfd[0]);
    ::close(fakefd[0]);
    ::close(infd[1]);
    ::close(outfd[1]);
    ::close(errfd[1]);
    ::close(fakefd[1]);
    exit(127);
  }
  try {
    int loop;
    int dummy;
    ::read(fakefd[0], &dummy, 1);	// simply wait for close of other end.
    ::close(fakefd[0]);
#if !defined(ACDK_OS_BSD)
    for (loop = 0; loop < 20; loop++) 
    {
        // wait a moment, so the child may exit after failed exec().
      Thread::sleep(250);
      if ((isRunning() == true) || (_exitCode != -1))
        break;
      DOUT("process is not startup. sleep a while");
    }
    if ((loop >= 20) || (_exitCode == 127)) 
    {
      THROW1(IOException, SBSTR("Process  didn't started up; exitCode: " << _exitCode));
    }
#endif 
  } catch (RRuntimeException ex) {
    // waitpid failed somehow...
    THROW1(IOException, "something awful happened while creating new Process.");
  }
#else // ACDK_OS_*
#error currently no process creation available for this platform/OS
#endif
  _status = ProcessStillRunning;
  _stdinWriter  = new FileWriter(iFD, true);
  _stdoutReader = new Process_PipeReader(oFD, true);
  _stderrReader = new Process_PipeReader(eFD, true);
  ::close(iFD->c_fd());
  ::close(oFD->c_fd());
  ::close(eFD->c_fd());
}

//virtual
RReader
Process::getErrorStream()
{
  return (RReader)_stderrReader;
}


//virtual
RWriter
Process::getInputStream()
{
  return (RWriter)_stdinWriter;
}

//virtual
RReader
Process::getOutputStream()
{
  return (RReader)_stdoutReader;
}


//virtual
int
Process::waitFor(int timeOutMs)
{
  if (_exitCode != -1)
    return _exitCode;

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  int ret;
  do {
    if (timeOutMs == -1)
      timeOutMs = INFINITE;
    ret = WaitForSingleObject(_processInfo.hProcess, timeOutMs);
    if (WAIT_OBJECT_0 == ret) 
    {
      DWORD ex;
      if ((GetExitCodeProcess(_processInfo.hProcess, &ex)) == 0)
        THROW1(IllegalThreadStateException, "GetExitCodeProcess Failed");
      if (ex== STILL_ACTIVE)
        continue;
      _exitCode = ex;
      _status = ProcessFinished;
      return _exitCode;
    }
    else if (WAIT_TIMEOUT == ret)
    {
      return ProcessStillRunning;
    }
  } while(ret == WAIT_OBJECT_0);
  THROW1(IOException, RString("WaitForSingleObject failed with ") + ret);
#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  int status;
  pid_t pid;
  DOUT("waitpid1 ...");
  jlong start = acdk::util::SysDate().getTime();
  jlong now = start;
  //sys::tick_t start = sys::core_tick::now();
  bool finished = false;
  int waitPidOptions = 0;
  if (timeOutMs != -1)
    waitPidOptions = WNOHANG;

  do {
    if ((pid = waitpid(_processId, &status, waitPidOptions)) != _processId && timeOutMs == -1)
    {
      RString msg = SBSTR("waitpid failed, got " << (int)pid << " instead of "  << (int)_processId);
      if (pid < 0)
        msg = msg + " errno is " + Integer::toString(errno);
      DOUT(msg);
      THROW1(IllegalThreadStateException, msg);
    }
    DOUT("waitpid status: " << status << "; _processId: " << (int)_processId << "; pid returned: " << (int)pid);
    if (timeOutMs != -1)
    {
#if defined(ACDK_OS_SOLARIS)
      if (pid == _processId && WIFEXITED(status) != 0)
#else
      if (WIFEXITED(status) != 0)
#endif
      {
        finished = true;
        break;
      }
      DOUT("Process::waitFor sleep 100");
      Thread::sleep(100);
    }
    now = acdk::util::SysDate().getTime();
    //int ms = sys::core_tick::millisecsDiff(start, sys::core_tick::now());
    /*
      DOUT("clock: " << (jlong)sys::core_tick::now() << "; sysTime: " << acdk::util::SysDate().getTime() 
         << "; Timeout: " << timeOutMs << "; now: " << ms << "; clockpermillisec: " 
         << int(CLOCKS_PER_MILLISECONDS) << "; clock per sec: " << int(CLOCKS_PER_SEC));
    */
  } while (timeOutMs != -1 && ((now - start) < timeOutMs));

  DOUT("waitpid returned: " << status);
#if defined(ACDK_OS_SOLARIS)
  if (pid == _processId && WIFEXITED(status) != 0)
#else
  if (WIFEXITED(status) != 0)
#endif
  {
    _exitCode = WEXITSTATUS(status);
    _status = ProcessFinished;
  }
  else if (WIFSIGNALED(status) && finished == true)
  {
    _exitCode = 128 | WTERMSIG(status);  // this is typical for shells
    _status = ProcessFinished;
  }
  else
    return ProcessStillRunning;

#else // ACDK_OS_*
#error currently no process creation available for this platform/OS
#endif
  return _exitCode;
}

bool
Process::isRunning()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  DWORD ex;
  GetExitCodeProcess(_processInfo.hProcess, &ex);
  if (ex == STILL_ACTIVE)
    return true;
#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  int killret;
  if ((killret = kill(_processId, 0)) == 0) 
  {
    int status = waitpid(_processId, &status, WNOHANG);
    DOUT("waitpid(" << _processId << ") returned: " << status);
    switch (status) 
    {
      case -1:
        THROW1(RuntimeException, "waitpid failed");
        break;
      case 0:  // isn't a zombie...
        break;
      default:  // was a zombie, save it's exit-status
        if (WIFEXITED(status))
          _exitCode = WEXITSTATUS(status);
        else if (WIFSIGNALED(status))
          _exitCode = 128 | WTERMSIG(status);  // this is typical for shells
        else
          _exitCode = 128;
/* maybe these will be implemented some day */
#if 0
        if (WIFSIGNALED(status))
          _termSig = WTERMSIG(status);
        if (WIFSTOPPED(status))
          _stopSig = WSTOPSIG(status);
#endif
        return false;
    }
    return true;
  }
  else
  {
    DOUT("Process::isRunning() kill returned: " << strerror(errno));
  }
#else // ACDK_OS_*
#error currently no process creation available for this platform/OS
#endif
  return false;
}


//static
int
Process::getProcessId()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  return (int)GetCurrentProcessId();
#elif defined(ACDK_OS_UNIX) || defined(ACDK_OS_CYGWIN32)
  return (int)getpid();
#else // ACDK_OS_*
#error currently no process creation available for this platform/OS
#endif
}

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
HINSTANCE
getKernel32()
{
  static HINSTANCE kernel32 = NULL;
  if (kernel32 != NULL)
    return kernel32;
  kernel32 = LoadLibraryA("kernel32.dll");
  return kernel32;
    
}
typedef WINBASEAPI HANDLE WINAPI OpenThreadFunc(DWORD dwDesiredAccess, BOOL bInheridedHandle, DWORD dwThreadID);
HANDLE openThread(DWORD dwDesiredAccess, BOOL bInheridedHandle, DWORD dwThreadID)
{
  HINSTANCE kernel32 = getKernel32();
    if (kernel32 == NULL)
      return NULL;

  static FARPROC openthread = NULL;
  if (openthread == NULL)
    openthread = GetProcAddress(kernel32, "OpenThread");
    if (openthread == NULL)
      return NULL;
  return ((OpenThreadFunc*)(void*)openthread)(dwDesiredAccess, bInheridedHandle, dwThreadID);
}
typedef WINBASEAPI BOOL WINAPI Thread32FirstFunc(HANDLE hSnapshot, LPTHREADENTRY32 lpte);
BOOL thread32First(HANDLE hSnapshot, LPTHREADENTRY32 lpte)
{
  HINSTANCE kernel32 = getKernel32();
  if (kernel32 == NULL)
    return NULL;
  static FARPROC thread32First = NULL;
  if (thread32First == NULL)
    thread32First = GetProcAddress(kernel32, "Thread32First");
  if (thread32First == NULL)
    return NULL;
  return ((Thread32FirstFunc*)(void*)thread32First)(hSnapshot, lpte);
  
}


BOOL thread32Next(HANDLE hSnapshot, LPTHREADENTRY32 lpte)
{
  HINSTANCE kernel32 = getKernel32();
  if (kernel32 == NULL)
    return NULL;
  static FARPROC thread32Next = NULL;
  if (thread32Next == NULL)
    thread32Next = GetProcAddress(kernel32, "Thread32Next");
  if (thread32Next == NULL)
    return NULL;
  return ((Thread32FirstFunc*)(void*)thread32Next)(hSnapshot, lpte);
  
}

typedef WINBASEAPI HANDLE WINAPI CreateToolhelp32SnapshotFunc(DWORD dwFlags, DWORD th32ProcessID);
HANDLE createToolhelp32Snapshot(DWORD dwFlags, DWORD th32ProcessID)
{
  HINSTANCE kernel32 = getKernel32();
  if (kernel32 == NULL)
    return NULL;
  static FARPROC createToolhelp32Snapshot = NULL;
  if (createToolhelp32Snapshot == NULL)
    createToolhelp32Snapshot = GetProcAddress(kernel32, "CreateToolhelp32Snapshot"); 
  if (createToolhelp32Snapshot == NULL)
    return NULL;
  return ((CreateToolhelp32SnapshotFunc*)(void*)createToolhelp32Snapshot)(dwFlags, th32ProcessID);
}

bool suspendResumeWin32Process(DWORD pid, bool resume)
{
  HANDLE hThreadSnap = createToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0); 
  if (hThreadSnap == INVALID_HANDLE_VALUE) 
    return false; 
  THREADENTRY32 te32 = { 0 }; 
  te32.dwSize = sizeof(THREADENTRY32); 

  if (thread32First(hThreadSnap, &te32) == FALSE) 
    return false;
  
  do { 
    if (te32.th32OwnerProcessID == pid) 
    {
      HANDLE hThread = openThread(THREAD_SUSPEND_RESUME, FALSE, te32.th32ThreadID);
			if (resume == true)
		    ResumeThread(hThread);
      else
			  SuspendThread(hThread);
		  CloseHandle(hThread);
    } 
  } while (thread32Next(hThreadSnap, &te32) == TRUE); 
  CloseHandle (hThreadSnap); 
  return true; 
}
#endif //defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)


void 
Process::suspend()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  suspendResumeWin32Process(_processInfo.dwProcessId, false);
#else
  int ret = kill(_processId, SIGSTOP);
#endif
}

void 
Process::resume()
{
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  suspendResumeWin32Process(_processInfo.dwProcessId, true);
#else
  int ret = kill(_processId, SIGCONT);
#endif
   
}

} // lang
} // acdk

