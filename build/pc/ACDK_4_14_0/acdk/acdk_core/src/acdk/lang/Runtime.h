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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Runtime.h,v 1.21 2005/04/09 19:26:50 kommer Exp $

#ifndef acdk_lang_Runtime_h
#define acdk_lang_Runtime_h

#include "Process.h"
#include <acdk/util/Properties.h>

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Runtime);
ACDK_DECL_CLASS(Process);
ACDK_DECL_INTERFACE(SignalEventHandler);

/**
  Which signal occurs
*/
enum SignalEventType
{
  /**
    SIGINT on unix machines
  */
  CtrlC_Event   = 0x0001, 
  /**
    on Unix SIGHUP
  */
  CtrlBreak_Event = 0x0002
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SignalEventType);

/**
  Interface to handle signals
  @see SignalEventType for which signals can be handled
*/
class ACDK_CORE_PUBLIC SignalEventHandler
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(SignalEventHandler)
public:
  /**
    @return if the handleEvent() returns true no more handler function
            are called.
  */
  virtual bool handleEvent(SignalEventType event) = 0;
  virtual bool equals(IN(RSignalEventHandler) other) = 0;
  virtual int listenToEvents() { return CtrlC_Event | CtrlBreak_Event; }

};

/** 
  Interface class to launch extern process
  @see acdk::cfgscript::ShellExecutor for a more easy to use
       class for launching external processes.
  API: Java extended<br>
  @author Roger Rene Kommer
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:50 $
*/  

class ACDK_CORE_PUBLIC Runtime
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Runtime)
  
public:
  static int SignaledCtrlCCount;
  static int SignaledCtrlBreakCount;
  static RSignalEventHandlerArray eventHandler;
  static RSignalEventHandlerArray getEventHandlers();
  

  static RProcess exec(IN(RString) command, IN(RString) workdir = Nil) ;
  static RProcess exec(IN(RStringArray) cmdarray, IN(RString) workdir = Nil);
  static RProcess exec(IN(RStringArray) cmdarray, IN(RStringArray) envp, IN(RString) workdir = Nil);
  static RProcess exec(IN(RStringArray) cmdarray, IN(acdk::util::RProperties) envp, IN(RString) workdir = Nil);
  static RProcess exec(IN(RString) command, IN(RStringArray) envp, IN(RString) workdir = Nil);
  static RProcess exec(IN(RString) command, IN(acdk::util::RProperties) envp, IN(RString) workdir = Nil);
  /**
    Note: not threadsafe 
  */
  static void registerSignalEventHandler(IN(RSignalEventHandler) handler);
  /**
    Note: not threadsafe 
  */
  static void unregisterSignalEventHandler(IN(RSignalEventHandler) handler);
  /**
    @param signals bitwise combinations of SignalEventType.
                    if 0 all are unregistered
  */
  static void unregisterAllEventHandler(int signals);
  void exit(int status);
  jlong freeMemory();
  void gc();
  static RRuntime getRuntime();
  void load(IN(RString) filename);
  void loadLibrary(IN(RString) libname);
  void runFinalization();
  jlong totalMemory();
  void traceInstructions(bool on);
  void traceMethodCalls(bool on);
  
  /** Initialize the signal settings */
  static void initSignals();
  static bool _traceMethodCalls;
};


} // lang
} // acdk 

#endif //acdk_lang_Runtime_h

