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
#include "ScriptDebug.h"
#include "ScriptException.h"
#include "ChDir.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace cfgscript {

using namespace acdk::lang::dmi;

bool 
FunctionEnterDebugPoint::doBreak(int action, IN(RExecutionStackFrame) frame)
{
  if ((action & DRAEnterFunction) == 0)
    return false;
  if (frame->_currentClazzMethodInfo == 0)
    return  false;
  const ClazzMethodInfo* mi = frame->_currentClazzMethodInfo;
  RString currentMethod = mi->toTypeString((const ClazzInfo*)mi->_scopeParent, TpFtJavaType | TpFtFqName | TpFtTypeDef);
  if (currentMethod->indexOf(_functionName) == -1)
    return  false;
  System::out->println("Break [" + _functionName + "] breaked at: " + currentMethod);
  // TODO break now
  return true;
}

bool  
SourceLineDebugPoint::doBreak(int action, IN(RExecutionStackFrame) frame)
{
  if (frame->getSourceLine() != _sourceLine)
    return false;
  if (_sourceName == Nil)
    return true;
  if (frame->getSourceFile()->indexOf(_sourceName) != -1)
    return true;
  return false;
}


//static 
RSourceLineDebugPoint 
SourceLineDebugPoint::parse(IN(RString) ident)
{
  if (ident == Nil || ident->length() == 0)
    return Nil;
  int idx = ident->indexOf(':');
  if (idx == -1)
  {
    RNumber num = Number::parseToIntegerNumber(ident, 10, true);
    if (num == Nil)
      return Nil;
    return new SourceLineDebugPoint(Nil, num->intValue());
  }
  RString source = ident->substr(0, idx);
  RString line = ident->substr(idx + 1);
  RNumber num = Number::parseToIntegerNumber(line, 10, true);
  if (num == Nil)
    return Nil;
  return new SourceLineDebugPoint(source, num->intValue());
}

ThrowExceptionDebugPoint::ThrowExceptionDebugPoint(IN(RString) exName)
: _exceptionName(exName)
{
  if (_exceptionName == Nil)
    _exceptionName = String::emptyString();
  Throwable::registerThrowListener(this);
}
ThrowExceptionDebugPoint::~ThrowExceptionDebugPoint()
{
  Throwable::unregisterThrowListener(this);
}

bool 
ThrowExceptionDebugPoint::onThrow(IN(RThrowable) ex, int line, IN(RString) file)
{
  bool doBreak = false;
  if (_exceptionName->length() == 0)
    doBreak = true;
  else
  {
    RString throwEx = ex->getClass()->getName();
    if (throwEx->indexOf(_exceptionName) != -1)
      doBreak = true;
  }
  if (doBreak == false)
    return true;
  RExecutionStackFrame frame = ExecutionStack::get()->top();
  frame->_scopeProps->setObjectVal("__activeException", &ex);
  
  PEStack stack(frame->_script, frame->_scopeProps);
  DebugNextAction action = DebugBreakPoints::get()->getDebugger()->onBreak(DRAThrowException, stack, frame);
  if (action == DbgNANoThrow)
    return false;
  return true;
}

WalkDebugPoint::WalkDebugPoint(IN(RExecutionStack) stack, DebugNextAction nextAction)
: _frame(stack->top())
, _nextAction(nextAction)
, _sourceLine(stack->top()->getSourceLine())
{
  if (_nextAction == DbgNAUntilReturn)
  {
    _frame = stack->getFrameFromTop(1);
  }
}

bool  
WalkDebugPoint::doBreak(int action, IN(RExecutionStackFrame) frame)
{
  if (_doBreakInternal(action, frame) == false)
    return false;
  DebugBreakPoints::get()->removeBreakpoint(this);
  return true;
}

bool isFrameInStack(IN(RExecutionStackFrame) frame)
{
  RExecutionStackFrameArray frames = ExecutionStack::get()->getFrames();
  for (int i = 0; i < frames->length(); ++i)
  {
    if (frames[i] == frame)
      return true;
  }
  return false;
}

bool 
WalkDebugPoint::_doBreakInternal(int action, IN(RExecutionStackFrame) frame)
{
  if (_nextAction == DbgNAStepOver)
  {
    if (_frame != frame)
    {
      if (isFrameInStack(_frame) == true)
        return false;
      // returned since _frame
      return true;
    }
      
    if (frame->getSourceLine() != _sourceLine)
      return true;
    return false;
  }
  else if (_nextAction == DbgNAStepInto)
  {
    if (_frame != frame)
      return true;
    if (frame->getSourceLine() != _sourceLine)
      return true;
    return false;
  }
  else if (_nextAction == DbgNAUntilReturn)
  {
    if (_frame == frame)
      return true;
    return false;
  }
  return false;
}


void debugHelp()
{
  System::out->println(
    "Debugging commands:\n"
    "running:\n"
    "c|continue           continue running\n"
    "n|next               execute next line\n"
    "s|step               next statement (step into functions)\n"
    "r|return             run until return\n"
    "q|quit               quit debugging/interpreter\n"
    
    "\nBreakpoints:\n"
    "br[f|l] <cmd> [<substr>] break at function or line\n"
    "    cmd: new|list|delete\n"
    "bre <cmd> [<ExceptionType>] break at throwing exception\n"
    
    "\nView Data:\n"
    "bt                   print backtrace\n"
    "l|list               view source\n"
    "df|dumpframe [frame] dump current scope or scope of frame number\n"
    "de|dumpenv           dump current environment\n"
    "trace [on|off]       activate or deactivate tracing\n"

    "\nview/modify:\n"
    "p|print <expr>       print expression\n"
    "i|input              interactive mode\n"
    
    "\n"
    "(Please refer to the CfgScript handbook for more information)\n"

  );
}

void printSourceLoc(PEStack& stack, int before = 0, int after = 0)
{
  RScript script = stack.script;
  int curTokenIdx = stack.getCurTokenIndex();
  //int curToken2 = ExecutionStack::getCurrentTokenIndex();
  //curToken = curToken2;
  RTokenizedSource tin = script->getTokenized();

  SourceToken& st = tin->getSourceToken(curTokenIdx);
  int lineno = st.sourcePos.linePos;
  int i;
  RString l;
  for (i = before; i >= 1; --i)
  {
    l = tin->getCodeOfLine(lineno - i);
    if (l != Nil)
    {
      System::out->println(SBSTR(lineno - i << ": " << l));
    }
  }
  l = tin->getCodeOfLine(lineno);
  l = l->trim();
  System::out->println("! " + l);
  for (i = 1; i < after; ++i)
  {
    l = tin->getCodeOfLine(lineno + i);
    if (l != Nil)
    {
      l = l->trim();
      System::out->println(SBSTR(lineno + i << ": " << l));
    }
  }
} 


void _getKeys(::acdk::util::TreeSet& keys, IN(RProps) scope, IN(RProps) frame)
{
  acdk::util::RIterator it = scope->keys();
  while (it->hasNext() == true)
  {
    keys.add(it->next());
  }
  if (scope == frame)
    return;
  RPropsArray pa = scope->getParentsProps();
  if (pa == Nil)
    return;
  for (int i = 0; i < pa->length(); ++i)
  {
    _getKeys(keys, pa[i], frame);
  }
}

void dumpProps(IN(RProps) scope, IN(RProps) frame)
{
  ::acdk::util::TreeSet keys;
  
  _getKeys(keys, scope, frame);
  
  int flags = PropsParentWrite;
  // copy from Props.cpp
  acdk::util::RIterator it = keys.iterator();
  RString ident = "  ";
  while (it->hasNext() == true)
  {
    RString k = RString(it->next());
    RDmiObject v = scope->get(k, flags);
    if (v->isStringType() == true)
    {
      System::out->println(ident + k + "=[\"" + v->toString() + "\"]");
    }
    /*else if (v->isObjectType() == true && instanceof(v->getObjectVar(), Props) == true)
    {
      
      if (dumpFlags & DumpWithChilds ||
          ((dumpFlags & DumpWithParent) && k->equals("_parent") == true))
      {
      if (usedkeys == Nil || k->equals("_parent") == true || usedkeys->contains(&k) == false)
      {
        if (usedkeys == Nil)
          usedkeys = new acdk::util::TreeSet();
        usedkeys->add(&k);
        System::out->println(ident + k + "=[");
        RProps(v->getObjectVar())->dump(dumpFlags, usedkeys, ident + "  ");
        System::out->println(ident + "]");
      }
      }
      
    }
    else*/
      System::out->println(ident + k + "=(" + v->getClass()->getName() + ")[" + v->toString() + "]");
  }

}

void dumpFrameProps(IN(RExecutionStackFrame) frame)
{
  dumpProps(frame->_scopeProps, frame->_frameProps);

}

void dumpProps(IN(RProps) props)
{
  props->dump();
}




OUT(RDebugBreakPoints) 
DebugBreakPoints::get()
{
  ACDK_STATIC_INSTANCE0(DebugBreakPoints, _breakPoints);
  return _breakPoints;
}

void traceCurLine(PEStack& stack)
{
  RScript script = stack.script;
  RTokenizedSource tin = script->getTokenized();
  int lineno = stack.curSourceToken().sourcePos.linePos;
  RString l = tin->getCodeOfLine(lineno + 1);
  if (l != Nil)
  {
    System::out->println(SBSTR(lineno + 1 << ": " << l));
  }
}
//foreign virtual 
bool 
ConsoleDebugger::doBreak(int action, PEStack& stack)
{
  return DebugBreakPoints::checkBreakPoints(action, stack);
}

//static 
bool 
DebugBreakPoints::doBreak(int action, PEStack& stack)
{
  RDebugger dbg = get()->_debugger;
  if (dbg == Nil)
    return false;
  bool erg =  dbg->doBreak(action, stack);
  if (erg == false)
    return false;
  return DebugBreakPoints::get()->onBreak(action, stack, ExecutionStack::getTop());
}

bool 
DebugBreakPoints::checkBreakPoints(int action, PEStack& stack)
{
  int debugFlags = ExecutionStack::getDebugFlags();
  if (debugFlags & DbgExitAll)
    return false;
  RExecutionStackFrame frame  = ExecutionStack::getTop();
  RDebugBreakPoints brk = get();
  if (brk->_wantBreak(action, frame) == true)
  {
    return true;
  }
  int dbgFlags = ExecutionStack::getDebugFlags();
  if (dbgFlags & DbgPrintEachLine)
  {
    traceCurLine(stack);
  }
  return false;
}

bool 
DebugBreakPoints::doBreakOnBreakPoint(int action, IN(RExecutionStackFrame) frame)
 {
  if (_debugPoints->length() == 0)
    return false;
  if (frame->_executionFlags & ESFFirstStatement)
    action |= DRAEnterFunction;

  for (int i = 0; i < _debugPoints->length(); ++i)
  {
    if (_debugPoints[i]->doBreak(action, frame) == true)
      return true;
  }
  return false;
}

RDebugPoint
DebugBreakPoints::removeBreakpoint(IN(RString) ident)
{
  RDebugPointArray da =  getBreakPoints();
  for (int i = 0; i < da->length(); ++i)
  {
    if (da[i]->isBreakPoint(ident) == true)
    {
      RDebugPoint ret = da[i];
      da->remove(i);
      return ret;
    }
  }
  return Nil;
}
void 
DebugBreakPoints::removeBreakpoint(IN(RDebugPoint) dbgPoint)
{
  RDebugPointArray da =  getBreakPoints();
  for (int i = 0; i < da->length(); ++i)
  {
    if (da[i] == dbgPoint)
    {
      da->remove(i);
      return;
    }
  }
}

bool 
DebugBreakPoints::_wantBreak(int action, IN(RExecutionStackFrame) frame)
{
  int dbgFlags = ExecutionStack::getDebugFlags();
  if (dbgFlags & DbgBreakStatements && action & DRANextStatement)
    return true;
  if (doBreakOnBreakPoint(action, frame) == true)
  {
    return true;
  }
  return false;
}

void interactive(PEStack& stack)
{
  System::out->println("enter code and end it with a single . in the line");
  StringBuffer sb;
  while (true)
  {
    RString inp = System::in->readLine();
    if (inp->equals(".") == true)
      break;
    sb << inp;
  }
  RScript dbgscript = new Script("<debug>", stack.script);
  int debugFlags = ExecutionStack::getDebugFlags();
  debugFlags &= ~DbgBreakStatements;
  ScopedDbgFlags scopeDbg(debugFlags);
  dbgscript->eval(sb.toString(), stack.props, PropsParentRead | PropsParentWrite);
}

namespace {
bool getCmdLine(IN(RString) line, IN(RString) shortCmd, IN(RString) longCmd, OUT(RString) argument, bool hasArg, bool trimArg)
{
  if (hasArg == false)
  {
    if (line->trim()->equals(shortCmd) == true)
      return true;
    if (longCmd != Nil && line->trim()->equals(longCmd) == true)
      return true;
  }
  bool found = false;
  if (line->startsWith(shortCmd) == true)
  {
    argument = line->substr(shortCmd->length());
  }
  else if (longCmd != Nil && line->startsWith(longCmd) == true)
  {
    argument = line->substr(longCmd->length());
  }
  else
    return false;
  if (trimArg == true)
    argument = argument->trim();
  return true;
}
} // anon namespace


bool
DebugBreakPoints::onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame)
{
  if (_debugger == Nil)
    return false;
  
  DebugNextAction nextAction = _debugger->onBreak(action, stack, ExecutionStack::getTop());
  setContinue();
  if (nextAction == DbgNAContinue)
    return false;
  else if (nextAction == DbgNAStepOver || nextAction == DbgNAStepInto || nextAction == DbgNAUntilReturn)
  {
    setContinue();
    DebugBreakPoints::get()->addBreakPoint(new WalkDebugPoint(ExecutionStack::get(), nextAction));
    return true;
  }
  else if (nextAction == DbgTerminate)
  { 

    ExecutionStack::get()->addDbgFlag(DbgExitAll);
    return true;
  }
  return false;
}

DebugNextAction
ConsoleDebugger::onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame)
{
    RScript script = stack.script;
    RDebugBreakPoints dbg = DebugBreakPoints::get();
    dbg->setBreakEachStmt();
    if (action == DRAThrowException)
    {
      RThrowable ex = (RThrowable)stack.props->getObjectVal("__activeException");
      System::out->println("Active Exception: " + ex->getClass()->getName() + ": " + ex->getMessage());
    }
    static RString lastInput = "";
nextLine:
    printSourceLoc(stack, 3, 0);
    
    while (true)
    {
      
nextInput:
    try {
      System::out->print("> ");
      System::out->flush();
      RString inp = System::in->readLine();
      if (inp->length() == 0)
        inp = lastInput;
      lastInput = inp;
      RString argument;
      if (getCmdLine(inp, "c", "continue", argument, false, true) == true)
      {
        DebugBreakPoints::get()->setContinue();
        return DbgNAContinue;
      }
      else if (getCmdLine(inp, "s", "step", argument, false, true) == true)
      {
        return DbgNAStepInto;
      }
      else if (getCmdLine(inp, "n", "next", argument, false, true) == true)
      {
        //dbg->addDbgFlag(DbgStepOverNextStmt);
        return DbgNAStepOver;
      }
       else if (getCmdLine(inp, "r", "return", argument, false, true) == true)
      {
        return DbgNAUntilReturn;
      }
      else if (getCmdLine(inp, "bt", "backtrace", argument, false, true) == true)
      {
        System::out->println(Script::getScriptBackTrace());
        goto nextInput;
      }
      else if (getCmdLine(inp, "l", "list", argument, false, true) == true)
      {
        printSourceLoc(stack, 5, 5);
        continue;
      }
      else if (getCmdLine(inp, "p ", "print ", argument, true, true) == true)
      {
        RString expr = argument;
        if (stack.props->hasValue(expr) == true)
        {
          if (instanceof(stack.props->get(expr)->getObjectVar(), Props) == true)
          {
            //RProps(stack.props->get(expr)->getObjectVar())->dump();
            dumpProps(RProps(stack.props->get(expr)->getObjectVar()));
            continue;
          }
        }
        expr = "acdk.lang.System.out.println((" + expr + ").toString());";
        RScript dbgscript = new Script("<debug>", stack.script);
        ScopedDbgFlags _nobreak(ExecutionStack::getDebugFlags() & ~DbgBreakStatements);
        dbgscript->eval(expr, stack.props, PropsParentRead | PropsParentWrite);
        continue;
      }
      else if (getCmdLine(inp, "df", "dumpframe", argument, true, true) == true)
      {
        RString num = argument;
        if (num == Nil || num->length() == 0)
        {
          dumpFrameProps(frame);
        }
        else
        {
          try {
            int st = Integer::parseInt(num);
            RExecutionStackFrame sf = ExecutionStack::get()->getFrameFromTop(st);
            if (sf == Nil)
            {
              System::out->println("index out of bounds");
            }
            else
              dumpFrameProps(sf);
          } catch (RNumberFormatException ex) {
            System::out->println(ex->getMessage());
          }
        }
        goto nextInput;
      }
      else if (getCmdLine(inp, "de", "dumpenv", argument, false, true) == true)
      //else if (inp->equals("de") == true)
      {
        stack.props->dump();
        goto nextInput;
      }
      else if (getCmdLine(inp, "brf ", Nil, argument, true, false) == true ||
              getCmdLine(inp, "brl ", Nil, argument, true, false) == true  ||
              getCmdLine(inp, "bre ", Nil, argument, true, false) == true )
      //else if (inp->startsWith("brf ") == true)
      {
        bool isBreakFunc = false;
        bool isBreakEx = false;
        if (inp->startsWith("brf") == true)
          isBreakFunc = true;
        if (inp->startsWith("bre") == true)
          isBreakEx = true;

        int idx = argument->indexOf(' ');
        RString arg;
        RString cmd = argument;
        if (idx != -1)
        {
          cmd = argument->substr(0, idx);
          arg = argument->substr(idx + 1);
        }
        if (cmd->equals("list") == true)
        {
          RDebugPointArray da =  DebugBreakPoints::get()->getBreakPoints();
          for (int i = 0; i < da->length(); ++i)
          {
            System::out->println(da[i]->toString());
          }
        }
        else if (cmd->equals("new") == true)
        {
          RDebugPoint dbp = Nil;
          if (isBreakFunc == true)
          {
            dbp = new FunctionEnterDebugPoint(arg);
            DebugBreakPoints::get()->addBreakPoint(dbp);
          }
          else if (isBreakEx == true)
          {
            dbp = new ThrowExceptionDebugPoint(arg);
            DebugBreakPoints::get()->addBreakPoint(dbp);
          }
          else
          {
            RSourceLineDebugPoint brp = SourceLineDebugPoint::parse(arg);
            if (brp == Nil)
            {
              System::out->println("Cannot create source line breakpoint, wrong format: " + arg);
              goto nextInput;
            }
            else
            {
              DebugBreakPoints::get()->addBreakPoint(&brp);
              dbp = &brp;
            }
          } 
          if (dbp != Nil)
          {
            System::out->println("Inserted new Debug break point [" + dbp->getIdentifier() + "]");
          }
        }
        else if (cmd->equals("delete") == true)
        {
          if (arg == Nil || arg->length() == 0)
          {
            DebugBreakPoints::get()->getBreakPoints()->resize(0);
            System::out->println("delete all break points");
          }
          else
          {
            RDebugPointArray da =  DebugBreakPoints::get()->getBreakPoints();
            for (int i = 0; i < da->length(); ++i)
            {
              if (da[i]->isBreakPoint(arg) == true)
              {
                System::out->println("Delete break point: " + da[i]->toString());
                da->remove(i);
                --i;
              }
            }
          }
        }
        else
        {
          System::out->println("unknown cmd for brf|brl|bre");
        }
        goto nextInput;
      }
      else if (inp->equals("trace off") == true)
      {
        dbg->removeDbgFlag(DbgPrintEachLine);
        continue;
      }
      else if (inp->equals("trace on") == true)
      {
        dbg->addDbgFlag(DbgPrintEachLine);
        continue;
      }
      else if (getCmdLine(inp, "i", "input", argument, false, false) == true)
      //else if (inp->equals("i") == true)
      {
        interactive(stack);
      }
      else if (getCmdLine(inp, "q", "quit", argument, false, false) == true)
      //else if (inp->equals("q") == true)
      {
        //dbg->addDbgFlag(DbgExitAll);
        return DbgTerminate;
      }
      else if (getCmdLine(inp, "_dumptoken", "_dumptoken", argument, false, false) == true)
      {
       
        RTokenizedSource tin = script->getTokenized();
        System::out->println(tin->_dumpTokens());
      } 
      else
      {
        debugHelp();
      }
    } catch (RThrowable ex) {
      System::out->println("Exception in evaluation: " + ex->getMessage());
    }
    }
  return DbgNAContinue; // never reached
}


} // namespace cfgscript
} // namespace acdk 
