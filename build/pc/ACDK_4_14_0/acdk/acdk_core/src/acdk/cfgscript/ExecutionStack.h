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

#ifndef acdk_cfgscript_ExecutionStack_h
#define acdk_cfgscript_ExecutionStack_h


#include "Props.h"
#include <acdk/io/CharReader.h>
#include <acdk/lang/ThreadLocal.h>
#include <acdk/lang/dmi/DmiDelegate.h>

#include "ScriptSource.h"
#include "SourceTokenizer.h"

namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::util::, Properties);

ACDK_DECL_CLASS(Script);


/**
  flags to control execution flow
*/
foreign
enum ExecutionFlags
{
  /**
    return current method
  */
  EFActiveReturn    = 0x01,
  /**
    NOT USED Exception is active
  */
  EFActiveException = 0x02,
  EFActiveBreak       = 0x04,
  EFActiveContinue    = 0x08,
  EFBreakableStatement    = 0x10,
  EFContinuableStatement  = 0x20
  
};

enum DebugFlags
{
  DbgRun                = 0x0000,
  DbgBreakStatements    = 0x0001,
  DbgStepInto           = 0x0002,
  DbgContinue           = 0x0004,
  DbgPrintEachLine      = 0x0008,
  /// not implemneted
  DbgBreakOnExecption   = 0x0010,
  /// not implemneted
  DbgBreakOnFail        = 0x0020,
  /// not implemneted
  DbgSysTraceOn         = 0x0100,
  /// internal
  DbgStepOverNextStmt   = 0x0200,
  DbgExitAll            = 0x0800,
  DbgScriptRunIsolated  = 0x1000
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, DebugFlags);




foreign enum ExecutionStackFlags
{
  ESFFirstStatement = 0x00001
};

struct PEStack;

/**
*/
ACDK_DECL_CLASS(ExecutionStackFrame);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC ExecutionStackFrame
: extends acdk::lang::Object
, implements acdk::lang::StackFrame
{
  ACDK_WITH_METAINFO(ExecutionStackFrame)
public:
  RScript _script;
  const acdk::lang::dmi::ClazzMethodInfo* _currentClazzMethodInfo;
  const acdk::lang::dmi::ClazzInfo* _currentClazzInfo;
  int _executionFlags;
  /// safed for backtrace
  int _curTokenIndex;
  RProps _frameProps;
  RProps _scopeProps;

  foreign ExecutionStackFrame(IN(RScript) script, int tokenIndex, const acdk::lang::dmi::ClazzMethodInfo* method, int executionFlags = 0);
  foreign ExecutionStackFrame(IN(RScript) script, int tokenIndex, const acdk::lang::dmi::ClazzInfo* clazzInfo, int executionFlags = 0);
  foreign ExecutionStackFrame(IN(RScript) script, int tokenIndex, int executionFlags = 0);

  foreign bool inConstructor()
  {
    return _currentClazzMethodInfo != 0 && _currentClazzMethodInfo->flags & acdk::lang::dmi::MiMiConstructor;
  }
  RString getScriptBackTrace(bool withSourcePos = true, bool withLocals = false);
  OUT(RProps) getFrameProps() { return _frameProps; }
  void setFrameProps(IN(RProps) prop)
  {
    _frameProps = prop;
  }
  OUT(RProps) getScopeProps() { return _frameProps; }
  void setScopeProps(IN(RProps) prop)
  {
    _scopeProps = prop;
  }
  virtual RString toString() { return SBSTR(getSourceFile() << ":" << getSourceLine()); }
  int getSourceLine();
  RString getSourceFile();

  // implementation from StackFrame
  virtual bool hasFileAndLine() { return true; }
  virtual bool hasLocals() { return true; }
  virtual bool hasFunctionSignature() { return _currentClazzMethodInfo != 0; }
  virtual bool hasCurrentSourceLine() { return true; }
  virtual bool hasLibararyName() { return true; }
  virtual bool hasMethod() { return _currentClazzMethodInfo != 0; }
  virtual bool isNative() { return false; }
  virtual int getFileLineNo();
  virtual RString getFileName();
  virtual RString getFunctionSignature();
  virtual RString getCurrentSourceLine();
  virtual RString getLibraryName();
  virtual acdk::lang::reflect::RMethod getMethod();
  virtual RStackFrameLocalArray getLocals();
};

ACDK_DECL_CLASS(ExecutionStack);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC ExecutionStack
: extends acdk::lang::Object
, implements acdk::lang::dmi::MetaObjectListener
{
  ACDK_WITH_METAINFO(ExecutionStack)
  RExecutionStackFrameArray _executeStack;
  
public:
  /**
    Throw exception on failure
    default is true
  */
  bool _throwOnFail;
  /**
    combination of DebugFlags
  */
  int _debugFlags;
  RExecutionStackFrame _nullFrame;

  acdk::lang::dmi::RMetaObjectArray _registeredMetaInfo;
  ExecutionStack() 
    : _executeStack(new ExecutionStackFrameArray(0))
    , _throwOnFail(true)
    , _debugFlags(0)
    , _nullFrame()
  {
  }
  ~ExecutionStack();
  static RExecutionStack get();
  int push(IN(RExecutionStackFrame) st)
  {
    _executeStack->append(st);
    return _executeStack->size() - 1;
  }
  void pop()
  {
    _executeStack->pop_back();
  }
  void pop(int idx)
  {
    while (_executeStack->size()  > idx)
      _executeStack->pop_back();
  }
  /**
    may return null if called directly from native
  */
  OUT(RExecutionStackFrame) top() 
  { 
    if (_executeStack->length() == 0)
      return _nullFrame;
    return _executeStack[_executeStack->length() - 1]; 
  }
  static void setCurrentTokenIndex(int index)
  {
    get()->top()->_curTokenIndex = index;
  }
  static bool throwOnFail()
  {
    return get()->_throwOnFail;
  }
  static OUT(RExecutionStackFrame) getTop() { return get()->top(); }
  static int getCurrentTokenIndex()
  {
    return getTop()->_curTokenIndex;
  }
  RString getScriptBackTrace(bool withLocals, bool withSourcePos);

  void breakToDebug() 
  {
    addDbgFlag(DbgBreakStatements); 
  }
  void addDbgFlag(int flag)
  {
    _debugFlags |= flag;
  }
  void removeDbgFlag(int flag)
  {
    _debugFlags &= ~flag;
  }

  static int getDebugFlags()
  {
    return get()->_debugFlags;
  }
  static void setDebugFlags(int flags)
  {
    get()->_debugFlags = flags;
  }
  /**
    0 is top
    @return Nil if out of bounds
  */
  foreign RExecutionStackFrame getFrameFromTop(int idx)
  {
    if (idx >= _executeStack->length())
      return Nil;
    return _executeStack[_executeStack->length() - (idx + 1)];
  }
  RExecutionStackFrameArray getFrames() { return _executeStack; }
  void startTransMetaInfo(int flags);
  /**
    after finished execution this method resets the stack 
    and deregister all tempory _registeredMetaInfo
  */
  void rollbackMetaInfo();
  /// implementation for MetaObjectListener
  virtual void onRegister(IN(acdk::lang::dmi::RMetaObject) metaObject);
  virtual void onUnregister(IN(acdk::lang::dmi::RMetaObject) metaObject);
protected:
  foreign bool calledByConstructor()
  {
    RExecutionStack es = get();
    if (_executeStack->size() < 2)
      return false;
    const acdk::lang::dmi::ClazzMethodInfo* mi = _executeStack[_executeStack->size() - 2]->_currentClazzMethodInfo;
    if (mi != 0 && mi->flags & acdk::lang::dmi::MiMiConstructor)
      return true;
    return false;
  }
  
};


} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_Script_h
