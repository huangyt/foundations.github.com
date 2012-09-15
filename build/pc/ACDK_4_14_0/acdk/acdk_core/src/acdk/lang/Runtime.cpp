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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Runtime.cpp,v 1.25 2005/04/09 19:26:50 kommer Exp $


#include <acdk.h>

#include "Runtime.h"

#include "System.h"
#include <acdk/io/FileDescriptor.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/util/Arrays.h>

#include "SharedLibrary.h"
#ifdef ACDK_OS_UNIX  
#include <signal.h>
#if defined(ACDK_OS_BSD) 
# define sighandler_t sig_t
#elif defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_DARWIN)

typedef void (*sighandler_t)(int) ;
#endif
#include <stdlib.h>
//#include <bits/signum.h>
#else

#endif 

namespace acdk {
namespace lang {


//static 
int Runtime::SignaledCtrlCCount = 0;
//static 
int Runtime::SignaledCtrlBreakCount = 0;


//static 
bool Runtime::_traceMethodCalls = false;

RRuntime __runtime;
//static 
RRuntime 
Runtime::getRuntime()
{
  if (__runtime == Nil) {
    __runtime = new Runtime();
    System::registerStaticReference(__runtime);
  }
  return __runtime;
}


RProcess 
Runtime::exec(IN(RString) command, IN(RString) workdir) 
{
  RProcess p = new Process(command, workdir);
  return p;
}

RProcess 
Runtime::exec(IN(RStringArray) cmdarray, IN(RString) workdir)
{
  RProcess p = new Process(cmdarray, workdir);
  return p;
}

RProcess 
Runtime::exec(IN(RStringArray) cmdarray, IN(RStringArray) envp, IN(RString) workdir)
{
  RProcess p = new Process(cmdarray, envp, workdir);
  return p;
}
 
RProcess 
Runtime::exec(IN(RString) command, IN(RStringArray) envp, IN(RString) workdir)
{
  RProcess p = new Process(command, envp, workdir);
  return p;
}

RStringArray
propsToEnvStringArray(IN(acdk::util::RProperties) envp)
{
  if (envp == Nil)
    return Nil;
  acdk::util::RIterator it = envp->propertyNames();
  RStringArray ret = new StringArray(0);
  while (it->hasNext() == true)
  {
    RString key = (RString)it->next();
    RString val = envp->getProperty(key);
    ret->append(SBSTR(key << "=" << val));
  }
  return ret;
}

//static 
RProcess 
Runtime::exec(IN(RStringArray) cmdarray, IN(acdk::util::RProperties) envp, IN(RString) workdir)
{
  return exec(cmdarray, propsToEnvStringArray(envp), workdir);
}

RProcess 
Runtime::exec(IN(RString) command, IN(acdk::util::RProperties) envp, IN(RString) workdir)
{
  return exec(command, propsToEnvStringArray(envp), workdir);
}
 
void 
Runtime::exit(int status)
{
  System::exit(status);
}

jlong 
Runtime::freeMemory()
{
  return (jlong)-1;
}

void 
Runtime::gc()
{
  System::gc();
}

//not supported: RInputStream getLocalizedInputStream(InputStream in) 
// not supported: ROutputStream getLocalizedOutputStream(OutputStream out) 
void 
Runtime::load(IN(RString) filename) //## may check path
{
  loadLibrary(filename);
} 
 
void 
Runtime::loadLibrary(IN(RString) libname)
{
  SharedLibrary slib(libname);
  slib.loadLibary();
}
 
void 
Runtime::runFinalization()
{

}

jlong 
Runtime::totalMemory()
{
  return (jlong)-1;
}
 
void 
Runtime::traceInstructions(bool on)
{
  // no consequences
}
 
void 
Runtime::traceMethodCalls(bool on)
{
  Runtime::_traceMethodCalls = on;
}


//static 
void 
Runtime::initSignals()
{
#ifdef ACDK_OS_UNIX  
  for (int i = 0; i <= 31 ; i++) {
    if (i != SIGUSR1 && 
        i != SIGUSR2 &&	// linux-pthread
        i != SIGTSTP && // CTRL-Z
        i != SIGCONT && // continue after STOP
        i != SIGCHLD && // wait()
        i != SIGINT)	// CTRL-C
      signal(i, SIG_IGN);
  }
#endif
}

//static 
RSignalEventHandlerArray Runtime::eventHandler;

//static 
RSignalEventHandlerArray 
Runtime::getEventHandlers()
{
  if (eventHandler != Nil)
    return eventHandler;
  eventHandler = new SignalEventHandlerArray(0);
  // ## not working on linux wy ever System::registerStaticReference(eventHandler);
  //System::registerStaticReference(eventHandler);
  return eventHandler;
}


#if defined(ACDK_OS_WIN32)
extern "C" BOOL WINAPI HandlerRoutine(DWORD dwCtrlType)
{
  
  SignalEventType event;
  if (dwCtrlType == CTRL_C_EVENT)
  {
    event = CtrlC_Event;
    ++Runtime::SignaledCtrlCCount;
  } else if (dwCtrlType == CTRL_BREAK_EVENT) {
    event = CtrlBreak_Event;
    ++Runtime::SignaledCtrlBreakCount;
  } else
    return FALSE;

  RSignalEventHandlerArray eha = Runtime::getEventHandlers();
  

  for (int i = 0; i < eha->length(); ++i)
  { 
    if (eha[i] != Nil && eha[i]->handleEvent(event) == true)
      return TRUE;
  }
  return FALSE;
}
#endif
#ifdef ACDK_OS_UNIX

sighandler_t oldSigInt = (sighandler_t)0;
sighandler_t oldSigHup = (sighandler_t)0;

extern "C" void sighandler(int signum)
{
  SignalEventType event;
  if (signum == SIGINT)
  {
    event = CtrlC_Event;
    ++Runtime::SignaledCtrlCCount;
  } else if (signum == SIGHUP) {
    event = CtrlBreak_Event;
    ++Runtime::SignaledCtrlBreakCount;
  } else
    return;

  RSignalEventHandlerArray eha = Runtime::getEventHandlers();
  for (int i = 0; i < eha->length(); ++i)
  { 
    if (eha[i] != Nil && eha[i]->handleEvent(event) == true)
      return;
  }
  if (signum == SIGINT)
    oldSigInt(signum);
  else if (signum == SIGHUP)
    oldSigHup(signum);
  return;
}
#endif //ACDK_OS_UNIX
//static 
void 
Runtime::registerSignalEventHandler(IN(RSignalEventHandler) handler)
{
  RSignalEventHandlerArray eha = getEventHandlers();
  if (eha->length() == 0)
  {
#if defined(ACDK_OS_WIN32)
    SetConsoleCtrlHandler(HandlerRoutine, TRUE);
#else
    oldSigInt = signal(SIGINT, sighandler);
    oldSigHup = signal(SIGHUP, sighandler);
#endif
  }
  eha->append(handler);
}

//static 
void 
Runtime::unregisterSignalEventHandler(IN(RSignalEventHandler) handler)
{
  RSignalEventHandlerArray eha = getEventHandlers();
  Runtime::eventHandler = ::acdk::util::Arrays::removeFirstElement(eha, handler);
  if (eha->length() == 0)
  {
#if defined(ACDK_OS_WIN32)
    SetConsoleCtrlHandler(HandlerRoutine, FALSE);
#else
    signal(SIGINT, oldSigInt);
    signal(SIGHUP, oldSigHup);
#endif
    Runtime::eventHandler = Nil;
  }
}

//static 
void 
Runtime::unregisterAllEventHandler(int signals)
{
  RSignalEventHandlerArray eha = Runtime::eventHandler;
  if (eha == Nil)
      return;
  int oldlenght = eha->length();
  for (int i = 0; i < eha->length(); ++i)
  {
    if (signals == 0 || eha[i] != Nil && eha[i]->listenToEvents() & signals)
    {
      eha = ::acdk::util::Arrays::removeElement(eha, i);
      --i;
    }
  }
  if (oldlenght > 0 && eha->length() == 0)
  {
#if defined(ACDK_OS_WIN32)
    SetConsoleCtrlHandler(HandlerRoutine, FALSE);
#else
    signal(SIGINT, oldSigInt);
    signal(SIGHUP, oldSigHup);
#endif
    Runtime::eventHandler = Nil;
  }
}

} // lang
} // acdk









