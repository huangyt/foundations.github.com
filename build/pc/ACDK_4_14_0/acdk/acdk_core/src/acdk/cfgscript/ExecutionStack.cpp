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


#include "Script.h"
#include "ScriptException.h"
#include "ChDir.h"
#include "ScriptEval.h"
#include "ScriptObject.h"
#include "ScriptDebug.h"

#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/reflect/Enumeration.h>

namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::io::, StreamTokenizer);
using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;

//ThreadLocal Script::_currentStack;

OUT(RThreadLocal)
getStackThreadLocal()
{
  static RThreadLocal _stack = new ThreadLocal();
  System::registerStaticReference(_stack);
  return _stack;
}

RExecutionStack 
ExecutionStack::get()
{
  OUT(RThreadLocal) _stack = getStackThreadLocal();
  if (_stack->get() == Nil)
  {
    _stack->set(new ExecutionStack());
  }
  return (RExecutionStack)_stack->get();
}

ExecutionStackFrame::ExecutionStackFrame(IN(RScript) script, int tokenIndex, const acdk::lang::dmi::ClazzMethodInfo* method, int executionFlags)
: _script(script)
, _currentClazzMethodInfo(method)
, _currentClazzInfo(0)
, _executionFlags(executionFlags)
, _curTokenIndex(tokenIndex)
{
  if (_currentClazzMethodInfo != 0)
  {
    _currentClazzInfo = (const acdk::lang::dmi::ClazzInfo*)_currentClazzMethodInfo->_scopeParent;
  }
}

ExecutionStackFrame::ExecutionStackFrame(IN(RScript) script, int tokenIndex, const acdk::lang::dmi::ClazzInfo* clazzInfo, int executionFlags)
: _script(script)
, _currentClazzMethodInfo(0)
, _currentClazzInfo(clazzInfo)
, _executionFlags(executionFlags)
, _curTokenIndex(tokenIndex)
{
}

ExecutionStackFrame::ExecutionStackFrame(IN(RScript) script, int tokenIndex, int executionFlags)
: _script(script)
, _currentClazzMethodInfo(0)
, _currentClazzInfo(0)
, _executionFlags(executionFlags)
, _curTokenIndex(tokenIndex)
{
}

RString 
ExecutionStackFrame::getScriptBackTrace(bool withSourcePos, bool withLocals)
{
  // ### TODO implement withLocals
  RTokenizedSource tsource = _script->getTokenized();
  StringBuffer sb;
  SourceToken& stk = tsource->getSourceToken(_curTokenIndex);

  sb << _script->getFileName()
#if defined(ACDK_OS_WIN32)
     << "(" 
#else
     << ":"
#endif
     << stk.sourcePos.linePos + 1 
#if defined(ACDK_OS_WIN32)
     << ")" 
#endif
     <<  ": " << tsource->getCodeOfLine(stk.sourcePos.linePos) << "\n";
   if (_currentClazzMethodInfo != 0)
  {
    _currentClazzMethodInfo->toTypeString(sb, (const acdk::lang::dmi::ClazzInfo*)_currentClazzMethodInfo->_scopeParent, acdk::lang::dmi::TpFtJavaType);
    sb << "\n";
  }
  return sb.toString();
}

int 
ExecutionStackFrame::getSourceLine()
{
  SourceToken& stk = _script->getTokenized()->getSourceToken(_curTokenIndex);
  return stk.sourcePos.linePos;
}

RString 
ExecutionStackFrame::getSourceFile()
{
  return _script->getFileName();
}

int 
ExecutionStackFrame::getFileLineNo()
{
  // ### todo wrong should be FileLineNo of beginning of method
  return getSourceLine();
}

RString 
ExecutionStackFrame::getFileName()
{
  return getSourceFile();
}

RString 
ExecutionStackFrame::getFunctionSignature()
{
  if (_currentClazzMethodInfo == 0)
    return "<unknown>";

  StringBuffer sb;
  _currentClazzMethodInfo->toTypeString(sb, (const acdk::lang::dmi::ClazzInfo*)_currentClazzMethodInfo->_scopeParent, acdk::lang::dmi::TpFtJavaType);
  return sb.toString();
}

RString 
ExecutionStackFrame::getCurrentSourceLine()
{
  RTokenizedSource tsource = _script->getTokenized();
  StringBuffer sb;
  SourceToken& stk = tsource->getSourceToken(_curTokenIndex);
  //sb << _script->getFileName() << "(" << stk.sourcePos.linePos + 1 << "): " << tsource->getCodeOfLine(stk.sourcePos.linePos) << "\n";
  return tsource->getCodeOfLine(stk.sourcePos.linePos);
}

RString 
ExecutionStackFrame::getLibraryName()
{
  return getSourceFile();
}

acdk::lang::reflect::RMethod 
ExecutionStackFrame::getMethod()
{
  if (_currentClazzMethodInfo == 0)
    return Nil;
  return new acdk::lang::reflect::Method((const acdk::lang::dmi::ClazzInfo*)_currentClazzMethodInfo->_scopeParent, _currentClazzMethodInfo);
}

RStackFrameLocalArray 
ExecutionStackFrame::getLocals()
{
  // ### TODO implement me
  return new StackFrameLocalArray(0);
}


ExecutionStack::~ExecutionStack() 
{ 
  for (int i = _executeStack->length() - 1; _executeStack->length() > 0; --i)
  {
    RExecutionStackFrame fr =  _executeStack[i];
    _executeStack->remove(i);

  }
  // if this done by automatic ~ExecutionStack, _throwObjectInsane happen in props hold by frame
  _executeStack = Nil;
  
}

RString
ExecutionStack::getScriptBackTrace(bool withSourcePos, bool withLocals)
{
  StringBuffer sb;
  for (int i = _executeStack->length() - 1; i >= 0; --i)
    sb << _executeStack[i]->getScriptBackTrace(withSourcePos, withLocals);
  return sb.toString();
}

void 
ExecutionStack::startTransMetaInfo(int flags)
{
  if ((flags & DbgScriptRunIsolated) == 0)
    return;

  _debugFlags |= DbgScriptRunIsolated;
  _registeredMetaInfo = new  acdk::lang::dmi::MetaObjectArray(0);
  acdk::lang::dmi::MetaObject::registerListener(this);
  
}

void deregisterMetaObject(IN(acdk::lang::dmi::RMetaObject) mo)
  {
    if (instanceof(mo, Class) == true)
    {
      RClass cls = (RClass)mo;
      const acdk::lang::dmi::ClazzInfo* ci = cls->objectClazzInfo();
      if (ScriptObject::_isScriptInterface(ci) == false)
        return;
      const_cast<acdk::lang::dmi::ClazzInfo*>(ci)->dispose();
    }
    else if (instanceof(mo, acdk::lang::reflect::Enumeration) == true)
    {
      acdk::lang::reflect::REnumeration ei = (acdk::lang::reflect::REnumeration)mo;
      if (ei->hasMetaAttribute("_cfgscript_script") == true)
      {
        const_cast<acdk::lang::dmi::ClazzEnumInfo*>(ei->getClazzEnumInfo())->dispose();
      }
    }
  }


void
ExecutionStack::rollbackMetaInfo()
{
  if ((_debugFlags & DbgScriptRunIsolated) == 0)
    return;
  acdk::lang::dmi::MetaObject::unRegisterListener(this);
  if (_registeredMetaInfo == Nil)
    return;
  for (int i = 0; i < _registeredMetaInfo->length(); ++i)
    deregisterMetaObject(_registeredMetaInfo[i]);
}
void 
Script::clearStack()
{
  getStackThreadLocal()->set(Nil);
}

void 
ExecutionStack::onRegister(IN(acdk::lang::dmi::RMetaObject) metaObject)
{
  if (_registeredMetaInfo == Nil)
    return;
  _registeredMetaInfo->append(metaObject);
}

void 
ExecutionStack::onUnregister(IN(acdk::lang::dmi::RMetaObject) metaObject)
{
}


} // namespace cfgscript
} // namespace acdk


