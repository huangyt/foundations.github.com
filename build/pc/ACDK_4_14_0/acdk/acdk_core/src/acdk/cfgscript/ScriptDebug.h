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

#ifndef acdk_cfgscript_ScriptDebug_h
#define acdk_cfgscript_ScriptDebug_h


#include <acdk/io/CharReader.h>
#include <acdk/lang/ThreadLocal.h>

#include "ScriptSource.h"
#include "SourceTokenizer.h"

namespace acdk {
namespace cfgscript {


/**
  current state of the interpreter
*/
enum DebugRunAction
{
  DRANextStatement    = 0x01,
  DRAEnterFunction    = 0x02,
  DRAThrowException   = 0x04,
  DRAReEnterFunction  = 0x08
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, DebugRunAction);

/**
  instruction from the debugger to the interpreter
  what to do next
*/
enum DebugNextAction
{
  DbgNAContinue,
  DbgNAStepOver,
  DbgNAStepInto,
  DbgNAUntilReturn,
  /// only in throw exception break point
  DbgNANoThrow,
  DbgTerminate
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, DebugNextAction);

ACDK_DECL_CLASS(ExecutionStackFrame);

ACDK_DECL_CLASS(DebugPoint);

/**
  generic interface for Breakpoints
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC DebugPoint
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DebugPoint)
public:
  DebugPoint(){}
  /**
    @param action one of DebugRunAction
    @param frame current frame of interpreter
  */
  virtual bool doBreak(int action, IN(RExecutionStackFrame) frame) = 0;
  virtual RString toString() = 0;
  /** 
    return the indefier of the breakpoint.
    used to list and remove breakpoints
  */
  virtual RString getIdentifier() = 0;
  /** return true if this string identifies this breakpoint */
  virtual bool isBreakPoint(IN(RString) ident) = 0;
};


ACDK_DECL_CLASS(FunctionEnterDebugPoint);

/**
  breakpoint on function when entered
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC FunctionEnterDebugPoint
: extends DebugPoint
{
  ACDK_WITH_METAINFO(FunctionEnterDebugPoint)
private:
  RString _functionName;
public:
  FunctionEnterDebugPoint(IN(RString) functionName)
    : _functionName(functionName)
  {
  }
  bool  doBreak(int action, IN(RExecutionStackFrame) frame);
  virtual RString getIdentifier() { return "brf " + _functionName; }
  
  RString toString() { return "break at function: [" + getIdentifier() + "]"; }
  virtual bool isBreakPoint(IN(RString) ident)
  {
    return ident->equals(_functionName) == true;
  }
};

ACDK_DECL_CLASS(SourceLineDebugPoint);

/**
  breakpoint when script source line will be executed
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC SourceLineDebugPoint
: extends DebugPoint
{
  ACDK_WITH_METAINFO(SourceLineDebugPoint)
private:
  RString _sourceName;
  int _sourceLine;
public:
  SourceLineDebugPoint(IN(RString) sourceName, int sourceLine)
    : _sourceName(sourceName)
    , _sourceLine(sourceLine)
  {
  }
  bool  doBreak(int action, IN(RExecutionStackFrame) frame);
  RString getIdentifier() 
  { 
    if (_sourceName == Nil)
      return SBSTR("brl " << _sourceLine);
    return SBSTR("brl " << _sourceName << ":" << _sourceLine); 
  }
  RString toString() { return "break at line : [" + getIdentifier() + "]"; }
  virtual bool isBreakPoint(IN(RString) ident)
  {
    return ident->equals(getIdentifier()) == true;
  }
  /*
    try to parse
    number
    source:number
    @return Nil if cannot be parsed
  */
  static RSourceLineDebugPoint parse(IN(RString) ident);
};


ACDK_DECL_CLASS(ThrowExceptionDebugPoint);

/**
  breakpoint if exception will be thrown
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC ThrowExceptionDebugPoint
: extends DebugPoint
, implements ThrowListener
{
  ACDK_WITH_METAINFO(ThrowExceptionDebugPoint)
private:
  RString _exceptionName;
public:
  /**
    @param exName part of the classname of the exception
  */
  ThrowExceptionDebugPoint(IN(RString) exName);
  ~ThrowExceptionDebugPoint();
  bool  doBreak(int action, IN(RExecutionStackFrame) frame) { return false; }
  RString getIdentifier() { return "bre " + _exceptionName; }
  RString toString() { return "break at exception: [" + getIdentifier() + "]"; }
  virtual bool isBreakPoint(IN(RString) ident)
  {
    return ident->equals(getIdentifier()) == true;
  }
  // from ThrowListener interface
  virtual bool onThrow(IN(RThrowable) ex, int line, IN(RString) file);
};


ACDK_DECL_CLASS(WalkDebugPoint);

/**
  implementation for 'step' and 'next' debug actions
  This removes itself if debug its
*/

class ACDK_CFGSCRIPT_LIB_PUBLIC WalkDebugPoint
: extends DebugPoint
{
  ACDK_WITH_METAINFO(WalkDebugPoint)
private:
  RExecutionStackFrame _frame;
  DebugNextAction _nextAction;
  int _sourceLine;
public:
  WalkDebugPoint(IN(RExecutionStack) stack, DebugNextAction nextAction);
  bool  doBreak(int action, IN(RExecutionStackFrame) frame);
  RString getIdentifier() 
  { 
    return SBSTR(_nextAction << " " << _frame->toString()); 
  }
  RString toString() { return "break at line : [" + getIdentifier() + "]"; }
  virtual bool isBreakPoint(IN(RString) ident)
  {
    return ident->equals(getIdentifier()) == true;
  }
private:
  foreign bool _doBreakInternal(int action, IN(RExecutionStackFrame) frame);
};





ACDK_DECL_INTERFACE(Debugger);
ACDK_DECL_CLASS(DebugBreakPoints);

/**
  interface for a CfgScript Debugger
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC Debugger
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Debugger)
public:
  /** only ask if debugger should break */
  foreign virtual bool doBreak(int action, PEStack& stack) = 0;
  /** branch to debugger */
  foreign virtual DebugNextAction onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame) = 0;
};

ACDK_DECL_CLASS(ConsoleDebugger);

/**
  Implementation of a command line CfgScript debugger
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC ConsoleDebugger
: extends acdk::lang::Object
, implements Debugger
{
  ACDK_WITH_METAINFO(ConsoleDebugger)
public:
  ConsoleDebugger() {}
  foreign virtual bool doBreak(int action, PEStack& stack);
  foreign virtual DebugNextAction onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame);
};

/**
  contains the configured debugger breakpoints
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC DebugBreakPoints
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DebugBreakPoints)
private:
  RDebugPointArray _debugPoints;
  RDebugger _debugger;
  int _lastInspectedLine;
public:
  DebugBreakPoints()
    : _debugPoints(new DebugPointArray(0))
    , _debugger(new ConsoleDebugger())
    , _lastInspectedLine(-1)
  {
  }
  static OUT(RDebugBreakPoints) get();
  void addBreakPoint(IN(RDebugPoint) dbgPoint)
  {
    _debugPoints->append(dbgPoint);
  }
  OUT(RDebugPointArray) getBreakPoints() { return _debugPoints; }
  static bool checkBreakPoints(int action, PEStack& stack);
  /**
    return true if state has changed
  */
  static bool doBreak(int action, PEStack& stack);
  /**
    return true if state has changed
  */
  bool onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame);

  bool doBreakOnBreakPoint(int action, IN(RExecutionStackFrame) frame);
  bool _wantBreak(int action, IN(RExecutionStackFrame) frame);
  void addDbgFlag(int flag)
  {
    ExecutionStack::setDebugFlags(flag | ExecutionStack::getDebugFlags());
  }
  void removeDbgFlag(int flag)
  {
    ExecutionStack::setDebugFlags(ExecutionStack::getDebugFlags() & ~flag);
  }

  void setContinue()
  {
    removeDbgFlag(DbgBreakStatements);
  }
  void setBreakEachStmt()
  {
    addDbgFlag(DbgBreakStatements);
  }
  void setTraceOn()
  {
    addDbgFlag(DbgPrintEachLine);
  }
  void setTraceOff()
  {
    removeDbgFlag(DbgPrintEachLine);
  }
  /**
    remove breakpoint by identifier
    return Nil if not found
  */
  RDebugPoint removeBreakpoint(IN(RString) ident);
  void removeBreakpoint(IN(RDebugPoint) dbgPoint);
  RDebugger getDebugger() { return _debugger; }
  void setDebugger(IN(RDebugger) debugger) { _debugger = debugger; }
};

#if !defined(DOXYGENONLY)
struct ScopedDbgFlags
{
  int _sicFlags;
  ScopedDbgFlags(int newFlags)
  {
    _sicFlags = ExecutionStack::getDebugFlags();
    ExecutionStack::setDebugFlags(newFlags);
  }
  ~ScopedDbgFlags()
  {
    ExecutionStack::setDebugFlags(_sicFlags);
  }
};

#endif //!defined(DOXYGENONLY)

} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ScriptDebug_h
