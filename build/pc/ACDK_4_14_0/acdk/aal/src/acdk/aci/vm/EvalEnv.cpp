// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 



#include "../Compiler.h"
#include "vm.h"
#include "../ast/Terminal.h"

namespace acdk {
namespace aci {
namespace vm {

//static 
acdk::lang::dmi::ScriptVar VarScope::_nilScriptVar;
//static 
acdk::lang::sys::core_vector<EvalEnv*> EvalEnv::_envStack;

EvalEnv::EvalEnv(IN(RCompiler) compiler)
: _compiler(compiler)
, _pc(-1)
, _globals(new acdk::cfgscript::Props())

{
  ACDK_SAFE_CONSTRUCTOR();
  _afStack.push_back(ActivationFrame(this));
  compiler->initializeForEvaluation(this);
  _envStack.push_back(this);
}

EvalEnv::~EvalEnv()
{
  acdk::lang::sys::core_vector<EvalEnv*>::const_iterator it = _envStack.find(this);
  if (it == _envStack.end())
    return;
  _envStack.erase((acdk::lang::sys::core_vector<EvalEnv*>::iterator)it);
}

//static 
REvalEnv 
EvalEnv::getEvalEnv()
{
  if (_envStack.size() == 0)
    THROW1(Exception, "No Evaluation Env available");
  return _envStack.back();
}

void 
EvalEnv::execute(IN(RExecutableArray) oca)
{
  int olength = oca->length();
  while (pc() + 1 < olength)
  {
    oca[incpc()]->execute(this);
    if (pc() == -2)
      break;
  }
}

void 
EvalEnv::dumpStack(IN(acdk::io::RPrintWriter) out)
{
  out->print("   Stack: ");
  for (int i = 0; i < stackTopIdx(); ++i)
  {
    ScriptVar sv = get(i);
    if (sv.isReference(sv.type) == true)
      out->print("&");
    out->print(sv.toString() + " | ");
  }
  out->println("");
}


ActivationFrame::ActivationFrame(IN(REvalEnv) env)
: _retPc(-1)
, _stackTop(env->stackTopIdx())
, _exceptionReturned(false)
, _activeException(Nil)
{
  
}


acdk::lang::dmi::ScriptVar 
ActivationFrame::load(int idx)
{
  return _frameData[normalizeIdx(idx)];
}

acdk::lang::dmi::ScriptVar 
ActivationFrame::loadRef(int idx)
{
  return _frameData[normalizeIdx(idx)].inoutOf();
}

void 
ActivationFrame::prepareCallee(IN(REvalEnv) env, IN(RExecutableArray) oca)
{
  _retPc = env->pc();
  env->pc(-1);
  jlong iflags = env->pop();
  int argcount = EvalEnv::getArgCount(iflags);
  env->pop(); // either string fname or int with methodhash
  //RString call = env->pop().getStringVar();
  for (int i = 0; i < argcount; ++i)
  {
    push(env->pop());
  }
  _stackTop = env->stackTopIdx();
}

void 
ActivationFrame::throwException(IN(REvalEnv) env, IN(RThrowable) ex, bool returnedfromCall)
{
  _activeException = ex;
  if (returnedfromCall == false && _catchHandler.size() != 0)
  {
    env->push(inOf(_activeException));
    env->pc(_catchHandler.pop());
  }
  else
  {
    env->pc(_retPc == -1 ? -2 : _retPc);
  }
  _exceptionReturned = returnedfromCall;
}



void 
ActivationFrame::afterCallee(IN(REvalEnv) env)
{
  int st = env->stackTopIdx();
  if (st - 1 > _stackTop)
  {
    EOUT("Leaf ActivationFrame with too many Variables on stack");
    int i;
    for (i = _stackTop  - 1; i > _stackTop; --i)
    {
      EOUT("  " << env->get(i).toString()->c_str());
    }
  }
  if (_activeException != Nil)
  {
    if (env->activationFrameCount() >= 2)
    {
      ActivationFrame& af = env->af(-1);
      af._exceptionReturned = true;
      af._activeException = _activeException;
    }
  }
  env->pc(_retPc);
}

bool 
ActivationFrame::checkReturnedException(IN(REvalEnv) env)
{
  if (_exceptionReturned == false)
    return false;
  _exceptionReturned = false;
  if (_catchHandler.size() != 0)
  {
    env->push(inOf(_activeException));
    env->pc(_catchHandler.pop());
  }
  else
  {
    env->pc(_retPc == -1 ? -2 : _retPc);
  }
  return true;
}


void 
ActivationFrame::finallyEnd(IN(REvalEnv) env)
{
  if (_activeException == Nil)
    return;
  if (_catchHandler.size() != 0)
  {
    env->pc(_catchHandler.pop());
  }
  else
  {
    env->pop(); // remove thrown exception
    env->pc(-2);
  }
}

} // vm
} // aci
} // acdk

