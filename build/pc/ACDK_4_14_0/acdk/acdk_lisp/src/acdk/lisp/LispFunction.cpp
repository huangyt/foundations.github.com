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



#include "LispCode.h"
#include "LispEnvironment.h"
#include "LispException.h"

#include <acdk/io/File.h>

namespace acdk {
namespace lisp {


LispFunction::LispFunction(IN(RLispList) definition)
: _isDefun(true)
, _isMacro(false)
{
  if (definition == Nil)
    return;

  if (definition->car()->toCode()->equals("defun") == true)
  {
    _name = definition->get(1)->toCode();
    _definition = definition;
    return;
  } 
  if (definition->car()->toCode()->equals("defmacro") == true)
  {
    _name = definition->get(1)->toCode();
    _definition = definition;
    _isMacro = true;
    return;
  }
  // lambda
  _isDefun = false;
  _definition = definition;
  _name = "<anon>";
  
}

//virtual 
RString 
LispFunction::getHelpText()
{
  RStringBuffer sb = new StringBuffer();
  RLispList helpbegin = _definition->cdr()->cdr()->cdr();
  if (helpbegin->car() == Nil)
    return getDeclDefinition()->toCode();
  RLispVar helptext = helpbegin->car();
  // RString tstr = helptext->toCode();	// what's the sense of this?
  while (instanceof(helptext, LispAtom) == true) {
    sb->append(acdk::io::File::endOfLine());
    sb->append(helptext->toString());
    if ((helpbegin = helpbegin->cdr()) == Nil)
      break;
    helptext = helpbegin->car();
  }
  return getDeclDefinition()->toCode() + sb->toString();
}

//virtual 
RLispList 
LispFunction::getDeclDefinition()
{
  RLispList erg = new LispList();
  return erg->append(_definition->car())->append(_definition->cdr()->car())->append(_definition->cdr()->cdr()->car());
}

RLispVar 
evalMacro2(IN(RLispEnvironment) env, IN(RLispVar) body)
{
  RLispVar ct = body; 
  RString bt = ct->toCode();
  if (instanceof(ct, LispList) == true)
  {
    RLispList tl = RLispList(ct);
    if (tl->car() != Nil && tl->car()->toString()->equals("backquote") == true)
    {
      RLispVar erg = env->_eval(tl);
      RString evaluaged = erg->toCode();
      return erg;
    }
    while (tl != Nil)
    {
      tl->setCar(evalMacro2(env, tl->car()));
      tl = tl->cdr();
    }
    return ct;
  }
  if (instanceof(ct, LispSymbol) == true)
  {
    RLispVar v = env->lookupLocalVar(ct->toString());
    if (v != Nil)
    {
      RString tv = v->toCode();
      return v;
    } 
    
  }
  return ct;
}

//virtual 
RLispVar 
LispFunction::evalMacro(IN(RLispEnvironment) env, IN(RLispList) args)
{
  StackVarHolder _stack(env, true);

  
  if (env->trace() == true)
  {
    RString tstr = args->toCode();
    env->trace_begin("Eval Macro [" + tstr + "]");
  }
  RLispList targs = (RLispList)_definition->get(2);
  bool hasOptional = false;
  RLispList arguments = args->cdr();
  while (targs != Nil && arguments != Nil) 
  {
    if (targs->car() == Nil) 
    {
      break;
    }
    if (targs->car()->toString()->equals((RString)"&optional") == true) {
      hasOptional = true;
      targs = targs->cdr();
    } else if (targs->car()->toString()->equals((RString)"&rest") == true) {
      hasOptional = true;
      targs = targs->cdr();
      int len = arguments->length();
      RLispList rest = new LispList(len);
      for (int i = 0; i < len; i++) 
      {
        rest->set(i, arguments->car());
	      arguments = arguments->cdr();
      }
      env->bindLocal(targs->car()->toString(), (RLispVar)rest, true);
      break;
    }
    env->bindLocal(targs->car()->toString(), arguments->car(), true);
    targs = targs->cdr();
    arguments = arguments->cdr();
  }
  RLispList body = (RLispList)_definition->cdr()->cdr()->cdr()->clone();
  RString bodystr = body->toCode();
  RLispList bl = body;
  while (bl != Nil) 
  {
    bl->setCar(evalMacro2(env, bl->car()));
    bl = bl->cdr();
  }

  body = (RLispList)body->car();
  if (env->trace() == true)
  {
    RString bstr = body->toCode();
    env->trace_end("to [" + bstr + "]");
  }
  return &body;
}


//virtual 
RLispVar 
LispFunction::eval(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (_isMacro == true)
    return evalMacro(env, args);

  //RLispStackFrame outer = env->_stackFrame.top();
  StackVarHolder _stack(env, true); 
  RLispList targs;
  if (_isDefun == true)
     targs = (RLispList)_definition->get(2);
  else
    targs = (RLispList)_definition->get(1);
  
  RLispList arguments = args->cdr();
  /* // dbg
  RString tfuncargs = targs->toCode();
  RString tcallcargs = arguments->toCode();
  int funcargcount = targs->length();
  int callargcount = arguments->length();
  // dbgend
  */
  bool hasOptional = false;
  while (targs != Nil && arguments != Nil) {
    if (targs->car() == Nil) {
      break;
    }
    if (targs->car()->toString()->equals("&optional") == true) {
      hasOptional = true;
      targs = targs->cdr();
    } else if (targs->car()->toString()->equals("&rest") == true) {
      hasOptional = true;
      targs = targs->cdr();
      int len = arguments->length();
      RLispList rest = new LispList(len);
      for (int i = 0; i < len; i++) 
      {
        rest->set(i, env->eval(arguments->car()));
	      arguments = arguments->cdr();
        if (env->exitNow() == true)
          return Nil;
      }
      env->bindLocal(targs->car()->toString(), (RLispVar)rest, true);
      break;
    }
    env->bindLocal(targs->car()->toString(), env->eval(arguments->car()), true);
    targs = targs->cdr();
    arguments = arguments->cdr();
  }
  if (env->exitNow() == true)
    return Nil;
  if (targs != Nil && targs->car() == Nil)
    targs = Nil;
  if (targs != Nil && (targs->car()->toString()->equals((RString)"&optional") == true ||
                       targs->car()->toString()->equals((RString)"&rest") == true))
    hasOptional = true;
  if (hasOptional == false && (targs != Nil || arguments != Nil))
    THROW2(LispException, env, "Arg count not matching: Callee=[" + getDeclDefinition()->toCode() + "], args=[" + args->toCode() + "]");
  
  //if (_ownScope == true)
  env->_stackFrame.top()->setParent(Nil); // unlink upper stack frame
  RLispVar l = Nil;
  RLispList body;
  if (_isDefun == true)
    body = _definition->cdr()->cdr()->cdr();
  else
    body = _definition->cdr()->cdr();
  while (body != Nil) 
  {
    l = env->eval(body->car());
    if (env->returnNow() == true) {
      env->returnNow(false);
      return l;
    }
    if (env->exitNow() == true)
      return Nil;
    body = body->cdr();
  }
  return l;
}


} // namespace lisp
} // namespace acdk



