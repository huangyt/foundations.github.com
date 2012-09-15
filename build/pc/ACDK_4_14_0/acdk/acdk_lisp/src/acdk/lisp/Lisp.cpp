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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/Lisp.cpp,v 1.38 2005/03/27 13:54:29 kommer Exp $



#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/String.h>
#include <acdk/lang/Character.h>
#include <acdk/io/Storage.h>
#include <acdk/io/File.h>
#include <acdk/util/HashSet.h>
#include <acdk/util/HashMap.h>
#include <acdk/lang/dmi/ScriptVar.h>

#include "lisp.h"
#include "LispCode.h"
#include "LispEnvironment.h"
#include "LispException.h"
#include "LispObject.h"
#include "LispDmiClient.h"

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::lang::dmi;
using namespace acdk::lang::sys;
using namespace acdk::lang::reflect;
using namespace acdk::io;
//using namespace acdk::util;




bool 
isTrue(IN(RLispVar) cond)
{
  if (cond == Nil)
    return false;
  if (instanceof(cond, LispAtom) == true) {
    if (RLispAtom(cond)->val().getBoolVar() == false)
        return false;
  }
  return true;
}

void getDumpedStackFrame(IN(RHashMap) cf, StringBuffer& sb);
void getDumpedEnv(IN(acdk::util::RProperties) cf, StringBuffer& sb);

RLispVar 
lisp_dump(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->cdr() == Nil || args->cdr()->car() == Nil) {
    env->dumpEnv((::acdk::io::RCharWriter)env->out);
    return &env->t();
  }
  RLispVar a = args->cdr()->car();
  if (instanceof(a, LispSymbol) == true) {
    try {
      RString t = a->toString();
      if (t->startsWith("sf") == true) {
        t = t->substr(2);
        int l = 0;
        try {
          if (t->length() > 0) {
            if (t->equals("x") == true) {
              for (int i = 0; i < env->_stackFrame.size(); i++) {
                env->out->print(RString("LispStackFrame ") + i + ": ");
                env->out->println(env->_stackFrame.getFromTop(i + 1)->toString()); 
              }
            } else 
              l = Integer::parseInt(t);
          }
          if (t->equals("x") == false) {
            env->out->print("LispStackFrame " + t + ": ");
            env->out->println(env->_stackFrame.getFromTop(l + 1)->toString());
          }
        } catch (RThrowable) {
          env->out->println("invalid stackframe level");
          env->out->println(a->toCode());  
        }
        return &env->t();
      }
      if (t->startsWith("cs") == true) {
        t = t->substr(2);
        int l = 0;
        try {
          if (t->length() > 0) {
            l = Integer::parseInt(t);
            env->out->print("Call Stack " + t + ": ");
            env->out->println(env->_evalStack.getFromTop(l + 1)->toCode());
          } else {
            for (int i = 0; i < env->_evalStack.size(); i++) {
              env->out->print(RString("Call Stack ") + i + ": ");
              env->out->println(env->_evalStack.getFromTop(i + 1)->toCode());
            }
          }
        } catch (RThrowable) {
          env->out->println("invalid callstack level");
          env->out->println(a->toCode());  
        }
        return &env->t();
        
      }
      if (t->equals((RString)"globals") == true) {
        StringBuffer sb(1024);
        getDumpedStackFrame(env->globals(), sb);
        env->out->println("globals:");
        env->out->println(sb.toString());
        return &env->t();
      }
      if (t->equals((RString)"env") == true) {
        StringBuffer sb(1024);
        getDumpedEnv(env->environment(), sb);
        env->out->println("env:");
        env->out->println(sb.toString());
        return &env->t();
      }
      if (t->equals((RString)"defuns") == true) {
        StringBuffer sb(1024);
        getDumpedStackFrame(env->_staticFuncs(), sb);
        env->out->println("defuns:");
        env->out->println(sb.toString());
        return &env->t();
      }
    } catch (RThrowable ex) {
      env->out->println("<invalid dump specifier>");
    }
  } 
  env->out->println(a->toString());
  return &env->t();
}


bool 
convertArg(IN(RLispEnvironment) env, ScriptVar& target, IN(RLispVar) source)
{
  if (source == Nil) {
    target = ScriptVar(false);
    return true;
  }
  if (env->t() == source) {
    target = ScriptVar(true);
    return true;
  }
  if (instanceof(source, LispAtom)) {
    target = RLispAtom(source)->val();
  //::acdk::lang::System::out->println(RString("converted `") +  RLispAtom(source)->val().getTypeInfo() + "´ (" + RLispAtom(source)->val().getObjectVar()->getName() + ") to `" + target.getTypeInfo() + "´ (" + target.getObjectVar()->getName() + ")");
    return true;
  }
  if (instanceof(source, LispSymbol)) {
    target = (RObject)source->toString();
    return true;
  }
  if (instanceof(source, LispList)) {
    target = ScriptVar(RObject(source));
    return true;
  }
  if (instanceof(source, LispObject)) 
  {
    target = ScriptVar(RObject(source));
    return true;
  }
  RString t = source->toCode();
  return false;
}

bool 
convertArg(RLispVar& target, ScriptVar& source)
{
#if 1
  RObject obj;
  if ((source.type == ScriptVar::ObjectType) && ((obj = source.getObjectVar()) != Nil) && (instanceof(obj, LispVar) == true)) {
    target = (RLispVar)obj;
    return true;
  }
#endif
  target = new LispAtom(source);
  //::acdk::lang::System::out->println(RString("converted `") + source.getTypeInfo() + "´ (" + source.getObjectVar()->getName() + ") to `" + RLispAtom(target)->val().getTypeInfo() + "´ (" + RLispAtom(target)->val().getObjectVar()->getName() + ")");
  return true;
}

bool
convertArgs(IN(RLispEnvironment) env, IN(RLispList) largs, acdk::lang::dmi::ScriptVarArray& sargs)
{
  RLispList tl = largs;
  int i = 0;
  while (tl != Nil) {
    RLispVar t = tl->car();
    if (convertArg(env, sargs[i], t) == false)
      return false;
    tl = tl->cdr();
    i++;
  }
  return true;
}
RLispVar  lisp_invoke_static(IN(RLispEnvironment) env, IN(RLispList) args);

RString toClazzName(IN(RString) clsname)
{
  return clsname->replace('.', '/')->replace("::", "/");
}

RLispVar 
lisp_new(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "new needs at least 1 argument");
  //RLispSymbol nameofClass = (RLispSymbol)args->cdr()->car();
  RLispList constructorargs = args->cdr()->cdr();
  if (args->cdr()->car() == Nil) 
    THROW2(LispException, env, "first argument of new must not be Nil!");
  RString clsname = args->cdr()->car()->toString();
  clsname = toClazzName(clsname);
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    return Nil;
  if (cls->objectClazzInfo()->static_dispatch == 0)
    return Nil;
  //System::err->println(RString("new: ") + clsname);
  if (clsname->equals("acdk/lisp/LispAtom") && (constructorargs != Nil) && (constructorargs->car() == env->t()) && (constructorargs->cdr() == Nil))  // mmmmh, that's not very nice...
    return &env->t();
  ScriptVar ret;
  acdk::lang::dmi::ScriptVarArray sargs(constructorargs == Nil ? 0 : constructorargs->length());
  if (convertArgs(env, constructorargs, sargs) == false) {
    THROW2(LispException, env, "cannot convert arguments");
  } 
  RString constructorname = clsname;
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  LispDmiClient dmiclient;
  if (cls->objectClazzInfo()->static_dispatch(constructorname, ret, sargs,  dmiclient, Nil, 
                                                MiPublic | MiMiConstructor, cls->objectClazzInfo(), 0) == 0)
    return Nil;
  
  RLispVar lret;
  convertArg(lret, ret);
  return lret;
}

RLispVar 
lisp_define(IN(RLispEnvironment) env, IN(RLispList) args)
{
  return Nil;
}

//#define WITH_CACHED_FUNCADRESS 1

RLispVar 
lisp_invoke(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() < 3) 
    THROW2(LispException, env, RString("invoke needs at least 2 arguments: ") + args->toCode());
  
  args = args->cdr();
  RObject obj = (args->car() == Nil ? RObject(Nil) : args->car()->getObject());
  if (obj == Nil)
    THROW2(LispException, env, RString("invoke with obj == Nil: ") + args->toCode());
  args = args->cdr();
  RLispList rest = args;
  if (rest->car() == Nil) 
    THROW2(LispException, env, "invoke need methodname: [" + args->toCode() + "]");
  RLispSymbol funcSymbol;
  RLispVar func = rest->car();
  if (instanceof(func, LispSymbol) == true) 
    funcSymbol = (RLispSymbol)rest->car();

  RString funcname = func->toString();
  //RLispSymbol func = (RLispSymbol)rest->car();
  rest = rest->cdr();
  RLispList funcargs = rest;
  ScriptVar ret;
  /*
#ifdef ACDK_DEBUG
  RString argsascode;
  if (rest != Nil)
    argsascode = rest->toCode();
#endif //ACDK_DEBUG
    */
  acdk::lang::dmi::ScriptVarArray sargs(funcargs != Nil ? funcargs->length() : 0);
  if (convertArgs(env, funcargs, sargs) == false) 
    THROW2(LispException, env, "invoke has not compatible arguments: [" + args->toCode() + "]");
  try {

    const ::acdk::lang::dmi::ClazzMethodInfo* metinf = 0;
#ifdef WITH_CACHED_FUNCADRESS
    int idx = -1;
    if (funcSymbol == Nil) {
      idx = funcname->indexOf('#');
      if (idx != -1) {
        metinf  = (const ::acdk::lang::dmi::ClazzMethodInfo*)Integer::parseInt(funcname->substr(idx + 1));
        funcname = funcname->substr(0, idx);
      }
    }
#endif //WITH_CACHED_FUNCADRESS
    LispDmiClient dmiclient;
    metinf = obj->standardDispatch(funcname, ret, sargs,  dmiclient, Nil, 
                                                MiPublic, obj->getClazzInfo(), metinf);
    if (metinf == 0)
      THROW2(LispException, env, "Dispatching invoke to ACDK-Object failed: [" + args->toCode() + "]");
#ifdef WITH_CACHED_FUNCADRESS
    if (funcSymbol == Nil && idx == -1) {
      RLispAtom la = (RLispAtom)func;
      la->val()  = (RObject)RString(funcname + "#" + Integer::toString((int)(void*)metinf));
    }
#endif //WITH_CACHED_FUNCADRESS
  } catch (RLispException ex) {
    throw ex;
  } catch (RException ex) {
    RStringBuffer sb = new StringBuffer();
    sb->append("\n -> searched for ");
    sb->append(funcname);
    sb->append("(");
    int argc = 0;
    while (funcargs != Nil && (funcargs->car() || funcargs->cdr())) {
      if (argc++ > 0)
        sb->append(", ");
      if (funcargs->car()) {
        sb->append(funcargs->car()->getName());
	if (instanceof(funcargs->car(), LispAtom)) {
	  sb->append("<");
	  sb->append(RLispAtom(funcargs->car())->val().getTypeInfo());
	  sb->append(">");
	}
      } else
        sb->append("NIL");
      funcargs = funcargs->cdr();
    }
    sb->append(")");
    THROW2(LispException, env, RString("invoke failed: ") + ex->getMessage() + " [" + args->toCode() + "]" + sb->toString());
  }
  RLispVar lret;
  convertArg(lret, ret);
  return lret;
}


RLispVar 
lisp_invoke_static(IN(RLispEnvironment) env, IN(RLispList) args)
{
   if (args->length() < 3) 
    THROW2(LispException, env, "invoke-static needs at least 2 arguments");
  RLispVar nameofClass = args->cdr()->car();
  RLispVar nameofMethod = args->cdr()->cdr()->car();
  RLispList methodargs = args->cdr()->cdr()->cdr();
  RString clsname = nameofClass->toString();
  RString methodname = nameofMethod->toString();
  clsname = clsname->replace('.', '/');
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    return Nil;
  if (cls->objectClazzInfo()->static_dispatch == 0)
    return Nil;
  ScriptVar ret;
  acdk::lang::dmi::ScriptVarArray sargs(methodargs == Nil ? 0 : methodargs->length());
  if (convertArgs(env, methodargs, sargs) == false) {
    THROW2(LispException, env, "cannot convert arguments");
  } 
#if 0
  ClazzMethodInfo *mi = ::acdk::lang::Object::lookupMethod(methodname->c_str(), sargs, const_cast<ClazzInfo *>(cls->objectClazzInfo()));
  if (mi != 0) {
    ::acdk::lang::System::out->println(RString("calling ") + mi->java_signature + "() [" + mi->label + "]");
  }
#endif
  LispDmiClient dmiclient;
  if (cls->objectClazzInfo()->static_dispatch(methodname, ret, sargs,  dmiclient, Nil, 
                                                MiPublic | MiStatic, cls->objectClazzInfo(), 0) == 0)
    return Nil;
  RLispVar lret;
  convertArg(lret, ret);
  return lret;
}


RLispVar 
lisp_peek(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;  
  if (args->length() != 3) 
    THROW2(LispException, env, RString("peek needs 2 arguments: ") + args->toCode());
  
  args = args->cdr();
  RObject obj = (args->car() == Nil ? RObject(Nil) : args->car()->getObject());
  if (obj == Nil)
    THROW2(LispException, env, RString("peek with obj == Nil: ") + args->toCode());
  args = args->cdr();
  RLispList rest = args;
  if (rest->car() == Nil) 
    THROW2(LispException, env, "peek needs membername: [" + args->toCode() + "]");

  RString membername = rest->car()->toString();
  return new LispAtom(obj->peek(membername));
}

RLispVar
lisp_poke(IN(RLispEnvironment) env, IN(RLispList) arguments) 
{
  RLispList args = arguments;
  if (args->length() != 4) 
    THROW2(LispException, env, RString("poke needs 3 arguments: ") + args->toCode());
  args = args->cdr();
  RObject obj = (args->car() == Nil ? RObject(Nil) : args->car()->getObject());
  if (obj == Nil)
    THROW2(LispException, env, RString("poke with obj == Nil: ") + args->toCode());
  
  args = args->cdr();
  RLispList rest = args;
  if (rest->car() == Nil) 
    THROW2(LispException, env, "poke need membername: [" + args->toCode() + "]");
  RString membername = rest->car()->toString();
  RLispVar var = rest->cdr()->car();
  RLispAtom theval = Nil;
  if (var != Nil) {
    if (instanceof(var, LispAtom) == false)
      THROW2(LispException, env, "poke need an LispAtom to set: [" + args->toCode() + "]");
    theval = (RLispAtom)var;
  }
  if (theval == Nil) {
    ScriptVar sv = RObject();
    obj->poke(membername, sv);
  } else
    obj->poke(membername, theval->val());
  
  return &theval;
}

RLispVar
lisp_poke_static(IN(RLispEnvironment) env, IN(RLispList) args) // #### to implement
{
  if (args->length() != 4) 
    THROW2(LispException, env, "poke-static needs 3 arguments");
  RLispSymbol nameofClass = (RLispSymbol )args->cdr()->car();
  RLispList constructorargs = args->cdr()->cdr();
  RString clsname = nameofClass->toString();
  clsname = clsname->replace('.', '/');
  /*
    RClass cls = Class::forName(clsname);
  if (cls == Nil)
    return Nil;
  */
  RString fieldname = constructorargs->car()->toString();
  constructorargs = constructorargs->cdr();
  if (instanceof(constructorargs->car(), LispAtom) == false) {
    THROW2(LispException, env, "poke-static need an LispAtom to set: [" + args->toCode() + "]");
  }
  RLispAtom theval = (RLispAtom)constructorargs->car();
  if (theval == Nil) {
    ScriptVar sv = RObject();
    StdDispatch::poke_static(clsname, fieldname, sv);
  } else
    StdDispatch::poke_static(clsname, fieldname, theval->val());
  return &theval;
}



RLispVar 
lisp_peek_static(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "peek-static needs 2 arguments");
  RLispSymbol nameofClass = (RLispSymbol )args->cdr()->car();
  RLispList constructorargs = args->cdr()->cdr();
  RString clsname = nameofClass->toString();
  clsname = clsname->replace('.', '/');
  RString fieldname = constructorargs->car()->toString();
  return new LispAtom(StdDispatch::peek_static(clsname, fieldname));
  /*
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    return Nil;

  RString fieldname = constructorargs->car()->toString();
  return new LispAtom(cls->getStaticMember(fieldname));
  */
}

RLispVar 
lisp_if(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 3) 
    THROW2(LispException, env, "if needs at least 3 arguments");
  RLispList c = args->cdr();
  RLispVar cond = c->car();
  c = c->cdr();
  RLispVar ex1 = c->car();
  RLispVar ex2;
  if (c->cdr()) 
    ex2 = c->cdr()->car();
  if (isTrue(env->eval(cond)) == true)
    return env->eval(ex1);
  else
    return env->eval(ex2);
}


RLispVar 
lisp_cond(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispList c = args;
  while (c->cdr() != Nil) {
    c = c->cdr();
    if (instanceof(c->car(), LispList) == false)
      THROW2(LispException, env, "cond expects list as case branches");

    RLispList casebranch = (RLispList)c->car(); // test if this is really a list
    RLispVar cond = casebranch->car();
    casebranch = casebranch->cdr();
    if (isTrue(env->eval(cond)) == true) {
      RLispVar erg;
      while (casebranch != Nil) {
        erg = env->eval(casebranch->car());
        casebranch = casebranch->cdr();
      }
      return erg;
    }
  }
  return Nil;
}

RLispVar 
lisp_return(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispList c = args->cdr();
  RLispVar erg;
  if (c != Nil)
    erg = c->car();
  env->returnNow(true);
  return erg;
}

RLispVar 
lisp_getv(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2) 
    THROW2(LispException, env, "getv needs 1 arguments");
  RLispVar expr = args->cdr()->car();
  RLispVar e = env->eval(expr);
  if (e == Nil)
    return Nil;
  if (instanceof(e, LispAtom) == false)
    THROW2(LispException, env, RString("getv needs an atom as name, not ") + e->getName());
  return env->lookupVar(e->toString(), false);
}

RLispVar 
lisp_setv(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "setv needs 2 arguments");
  RLispVar symb = args->cdr()->car();
  RLispVar expr = args->cdr()->cdr()->car();
  RLispVar e = env->eval(expr);
  RString symbstr = env->eval(symb)->toString();
  env->bindGlobal(symbstr, e); 
  return e;
}

RLispVar 
lisp_setq(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "setq needs 2 arguments");
  RLispVar symb = args->cdr()->car();
  RLispVar expr = args->cdr()->cdr()->car();
  RLispVar e = env->eval(expr);
  RString symbstr = symb->toString();
  env->bindGlobal(symbstr, e); 
  return e;
}



RLispVar 
lisp_setf(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "setf needs 2 arguments");
  RLispVar symb = args->cdr()->car();
  RLispVar expr = args->cdr()->cdr()->car();
  RLispVar e = env->eval(expr);
  RString symbstr = symb->toString();
#if 0
  if ((expr == Nil) && (e == Nil))
    System::err->println(RString("setf: ") + symbstr + " = NIL results in NIL");
  else if (expr == Nil)
    System::err->println(RString("setf: ") + symbstr + " = NIL results in " + e->getName() + "::" + e->toString());
  else if (e == Nil)
    System::err->println(RString("setf: ") + symbstr + " = " + expr->getName() + "::" + expr->toString() + " results in NIL");
  else
    System::err->println(RString("setf: ") + symbstr + " = " + expr->getName() + "::" + expr->toString() + " results in " + e->getName() + "::" + e->toString());
#endif
  if (symbstr->charAt(0) == '*') {
    env->bindGlobal(symbstr, e); 
    return e;
  }
  if (Character::isUpperCase(symbstr->charAt(0)) == true) {
    bool allUpperCase = true;
    for (int i = 0; i < symbstr->length(); i++) {
      if (Character::isLowerCase(symbstr->charAt(i)) == true) {
        allUpperCase = false;
        break;
      }
    }
    if (allUpperCase == true)
      env->bindToEnv(symbstr, e); 
    else
      env->bindGlobal(symbstr, e); 
  } else
    env->bindLocal(symbstr, e); 
  return e;
}

RLispVar 
lisp_unpack(IN(RLispEnvironment) env, IN(RLispList) args)
{
   if (args->length() != 2) 
    THROW2(LispException, env, "unpack needs 1 arguments");
  RLispVar targ = args->cdr()->car();
  if (instanceof(targ, LispAtom) == true)
	  if (instanceof(targ->getObject(), LispVar) == true)
		return (RLispVar)targ->getObject();
  return targ;
}

//virtual 
RLispVar 
LispCallBack::eval(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (env->exitNow() == true)
    return Nil;
  if (_evalargs == true) {
    LispCode code;
    RLispList tl = args;
    while (tl != Nil) {
      code.append(env->eval(tl->car()));
      //if (env->returnNow() == true) {
      //  env->returnNow(false);
        // ??? return Nil;
      //}
      if (env->exitNow() == true)
        return Nil;
    
      tl = tl->cdr();
    }
    //RString tstr = code.code()->toCode();
    return _evalfunc(env, code.code());
  } 
  return _evalfunc(env, args);
}


RLispVar 
lisp_plus(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 1) 
    THROW2(LispException, env, "+ needs at least 2 arguments");
  RLispList l = args->cdr();
  ScriptVar serg(byte(0));
  while (l != Nil) {
    if (instanceof(l->car(), LispAtom) == false) 
      THROW2(LispException, env, "+ expects atoms as arguments: " + args->toCode());
    serg = serg.addition(RLispAtom(l->car())->val());
    l = l->cdr();
  }
  return new LispAtom(serg);
}

RLispVar 
lisp_minus(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 3) 
    THROW2(LispException, env, "- needs 2 arguments");
  RLispAtom f = RLispAtom(args->cdr()->car());
  RLispAtom s = RLispAtom(args->cdr()->cdr()->car());
  return new LispAtom(f->val().subtraction(s->val()));
}



RLispVar 
lisp_multiply(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "* needs at least 1 arguments");
  RLispList l = args->cdr();
  ScriptVar serg(1);
  while (l != Nil) {
    if (instanceof(l->car(), LispAtom) == false)
      THROW2(LispException, env, "* needs atom as arguments");
    serg = serg.multiply(RLispAtom(l->car())->val());
    l = l->cdr();
  }
  return new LispAtom(serg);
}

RLispVar 
lisp_divide(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 3) 
    THROW2(LispException, env, "/ needs 2 arguments");
  RLispAtom f = RLispAtom(args->cdr()->car());
  RLispAtom s = RLispAtom(args->cdr()->cdr()->car());
  return new LispAtom(f->val().divide(s->val()));
}

RLispVar 
lisp_modulo(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 3) 
    THROW2(LispException, env, "% needs 2 arguments");
  RLispAtom f = RLispAtom(args->cdr()->car());
  RLispAtom s = RLispAtom(args->cdr()->cdr()->car());
  return new LispAtom(f->val().modulo(s->val()));
}

RLispVar 
lisp_gt(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "> needs 2 arguments");
  RLispList l = args->cdr();
  RLispVar fvar = l->car();
  RLispVar svar = l->cdr()->car();
  if (instanceof(fvar, LispAtom) == false || instanceof(svar, LispAtom) == false)
    THROW2(LispException, env, "> needs atom as argument");
  if (RLispAtom(fvar)->val().greater_than(RLispAtom(svar)->val()).getBoolVar() == true)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_length(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2) 
    THROW2(LispException, env, "length needs 1 arguments");
  RLispList l = args->cdr();
  if (instanceof(l->car(), LispList) == false)
    THROW2(LispException, env, "length args should be a list");
  return new LispAtom(ScriptVar(RLispList(l->car())->length()));
}

RLispVar 
lisp_list(IN(RLispEnvironment) env, IN(RLispList) args)
{
  int i, len = args->length();
#if 0
  ::acdk::lang::System::out->println(RString("list has ") + ::acdk::lang::Integer::toString(len) + " args");
  for (i = 0; i < len; i++)
    if (args->get(i) != Nil)
      ::acdk::lang::System::out->println(::acdk::lang::Integer::toString(i) + ": " + args->get(i)->toString());
    else
      ::acdk::lang::System::out->println(::acdk::lang::Integer::toString(i) + ": Nil");
#endif
  if (len < 2) 
    return Nil; //THROW2(LispException, env, "length needs at least 1 arguments");
  RLispList l = new LispList(len - 1);
  for (i = 1; i < len; i++)
    l->set(i - 1, args->get(i));
  len = l->length();
#if 0
  ::acdk::lang::System::out->println(RString("new list has ") + ::acdk::lang::Integer::toString(len) + " elements");
  for (i = 0; i < len; i++)
    if (l->get(i) != Nil)
      ::acdk::lang::System::out->println(::acdk::lang::Integer::toString(i) + ": " + l->get(i)->toString());
    else
      ::acdk::lang::System::out->println(::acdk::lang::Integer::toString(i) + ": Nil");
#endif
  return &l;
}

RLispVar 
lisp_cons(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 3) 
    THROW2(LispException, env, "cons needs 2 arguments");
  RLispVar l = args->cdr()->cdr()->car();
  if (l != Nil && instanceof(l, LispList) == false)
    THROW2(LispException, env, "second arg of cons must be NIL or a list");
  return new LispList(args->cdr()->car(), (RLispList)l);
}



RLispVar 
lisp_car(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;

  if (args->length() != 2) 
    THROW2(LispException, env, "car needs 1 arguments");
  

  args = args->cdr();
  if (args->car() == Nil)
    return Nil;
  if (instanceof(args->car(), LispList) == false)
    THROW2(LispException, env, "arg of car must be a list");
  RLispList thelist = (RLispList)args->car();
  return thelist->car();
}

RLispVar 
lisp_cdr(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 2) 
    THROW2(LispException, env, "car needs 1 arguments");
  
  
  args = args->cdr();
  if (args->car() == Nil)
    return Nil;
  if (instanceof(args->car(), LispList) == false)
    THROW2(LispException, env, "arg of car must be a list");
  RLispList thelist = (RLispList)args->car();
  return &thelist->cdr();
}

/** 
  (append list el) // appends el to the list
*/

RLispVar 
lisp_append(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;

  if (arguments->length() != 3) 
    THROW2(LispException, env, "append needs 2 arguments");
  
  
  args = args->cdr();
  
  RLispVar sec = args->cdr()->car();
  RLispVar list = args->car();
  if (list == Nil)
    return new LispList(sec);
  if (instanceof(list, LispList) == false)
    THROW2(LispException, env, RString("arg 1 of append must be a list not a ") + list->getName());
  RLispList firstlist = (RLispList)list;
  firstlist->append(sec);
  return &firstlist;
}

RLispVar 
lisp_setnth(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 4) 
    THROW2(LispException, env, "setnth needs 3 arguments");
  
  args = args->cdr();
  
  RLispVar sec = args->cdr()->car();
  RLispVar thrd = args->cdr()->cdr()->car();
  if (args->car() == Nil)
    THROW2(LispException, env, "arg 1 of setnth can't be Nil");
  if (instanceof(args->car(), LispList) == false)
    THROW2(LispException, env, "arg 1 of setnth must be a list");
  RLispList firstlist = (RLispList)args->car();
  if (sec == Nil)
    THROW2(LispException, env, "arg 2 of setnth can't be Nil");
  if (instanceof(sec, LispAtom) == false)
    THROW2(LispException, env, "arg 2 of setnth must be a atom");
  if (RLispAtom(sec)->val().type != ::acdk::lang::dmi::ScriptVar::DoubleType)
    THROW2(LispException, env, "arg 2 of setnth must be a number");
  int idx = RLispAtom(sec)->val().getIntVar();
  if ((idx < 0) || (idx >= firstlist->length()))
    THROW2(LispException, env, RString("setnth: index ") + idx + " is out of range");
  firstlist->set(idx, thrd);
  return &firstlist;
}

RLispVar 
lisp_quote(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2) 
    THROW2(LispException, env, "length needs 1 arguments");

  RLispVar v = args->cdr()->car();
  //RString tstr1 = args->toCode();
  return (RLispVar)v->clone();
}

RLispVar 
lisp_backquote(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->cdr() == Nil)
    return Nil;
  //RString tstr1 = args->toCode();
  RLispList ll = (RLispList)lisp_quote(env, args);
  RLispList erg = ll;
  
  while (ll != Nil)
  {
    if (ll->car() != Nil && instanceof(ll->car(), LispList) == true)
    {
      RLispList llist(ll->car());
      RLispVar cmd = llist->car();
      if (cmd != Nil)
      {
        RString tstr = cmd->toString();
        if (tstr->equals("comma"))
        {
          //RString tevstr = llist->cdr()->car()->toCode();
          RLispVar erg = env->eval(llist->cdr()->car());
          ll->setCar(erg);
        } 
        else if (tstr->equals("commaat"))
        {

          RLispVar erg = env->eval(llist->cdr()->car());
          RLispList inlist = (RLispList) erg;
          ll->setCar(inlist->car());
          RLispList elist = ll->cdr();
          RLispList aplist = ll;
          inlist = inlist->cdr();
          while (inlist != Nil)
          {
            RLispList nl = new LispList(inlist->car());
            aplist->setCdr(nl);
            aplist = nl;
            inlist = inlist->cdr();
          }
          aplist->setCdr(elist);
        }


      }
    }
    ll = ll->cdr();
  }
  RString ertstr = erg->toCode();
  return &erg;
}

RLispVar 
lisp_comma(IN(RLispEnvironment) env, IN(RLispList) args)
{
  return &args;
}

RLispVar 
lisp_commaat(IN(RLispEnvironment) env, IN(RLispList) args)
{
  return &args;
}

RLispVar 
lisp_eval(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (env->exitNow() == true)
    return Nil;
  if (args->length() != 2) 
    THROW2(LispException, env, "eval needs 1 arguments");
  RLispVar a = args->get(1);
  StackVarHolder _stack(env);
  RLispVar erg = env->eval(a);
  return erg;
}

RLispVar 
lisp_let(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "let needs at least 1 argument");
  if (instanceof(args->get(1), LispList) == false)
    THROW2(LispException, env, "arg 1 of let must be a list");
  
  StackVarHolder _stack(env, true);
  RLispList locals = (RLispList)args->get(1);
  for (int i = 0; i < locals->length(); i++) {
    if (instanceof(locals->get(i), LispList) == false)
      THROW2(LispException, env, "locals in let of let must be a list");
    RLispList cdev = (RLispList)locals->get(i);
    if (cdev->length() != 2)
      THROW2(LispException, env, "locals in let must be a list of 2 elements");
    env->bindLocal(cdev->get(0)->toString(), cdev->get(1), true);
  }
  RLispVar l = Nil;
  for (int j = 2; j < args->length(); j++) {
    l = env->eval(args->get(j));
    if (env->exitNow() == true)
      return Nil;
    
  }
  return l;
}

RLispVar 
lisp_while(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "while needs at least 1 argument");
  /** while condition expr */
  RLispVar condition = args->cdr()->car();
  RLispVar expr;
  if (args->cdr()->cdr() != Nil)
    expr = args->cdr()->cdr()->car();
  RLispVar erg;
  while (isTrue(env->eval(condition)) == true) {
    //System::out->println("TRUE: " + condition->toCode());
    if (expr != Nil) {
      StackHolder<RLispVar> __evalStackHolder(env->_evalStack, expr);
      erg = env->eval(expr);
      if (env->returnNow() == true)
        return erg;
      if (env->exitNow() == true)
        return Nil;
      
    }
  }
  return erg;  
}

RLispVar 
lisp_listp(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2) 
    THROW2(LispException, env, "lisp needs 1 argument");
  if (instanceof(args->get(1), LispList) == true)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_instanceof(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 3)
    THROW2(LispException, env, "instanceof needs 2 argument");
  
  args = args->cdr();
  RLispVar ov = args->car();
  args = args->cdr();
  RLispVar tv = args->car();
  RObject obj = ov->getObject();
  
  RString clsname = tv->toString();
  
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    THROW2(LispException, env, "Class [" + clsname + "] is unknown");
  if (obj->getClass()->isAssignableFrom(cls) == true)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_isdef(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 2)
    THROW2(LispException, env, "isdef needs 1 argument");
  args = args->cdr();
  RLispVar ov = args->car();
  if (instanceof(ov, LispSymbol) == false)
    THROW2(LispException, env, "isdef needs a symbol as argument");
  if (env->lookupVar(ov->toString(), false) == Nil)
    return Nil;
  return &env->t();
}

RLispVar 
lisp_internal(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 2)
    THROW2(LispException, env, "internal needs 1 argument");
  args = args->cdr();
  RLispVar ov = args->car();
  if (instanceof(ov, LispSymbol) == true)
    return new LispAtom(RString("Symbol"));
  if (instanceof(ov, LispAtom) == true)
    return new LispAtom(RString("Atom"));
  if (instanceof(ov, LispList) == true)
    return new LispAtom(RString("List"));
  if (instanceof(ov, LispArray) == true)
    return new LispAtom(RString("Array"));
  return new LispAtom(RString("Unknown"));
}

RLispVar 
lisp_internalp(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 2)
    THROW2(LispException, env, "internalp needs 1 argument");
  args = args->cdr();
  RLispVar ov = args->car();
  if (ov == Nil)
    return Nil;
  if (instanceof(ov, LispSymbol) == true)
    return new LispAtom(((RLispSymbol)ov)->toString());
  if (instanceof(ov, LispAtom) == true)
  {
    RString str = (((RLispAtom)ov)->val().getObjectVar() == Nil)? RString("NIL") : ((RLispAtom)ov)->val().getObjectVar()->getName();
    return new LispAtom(str);
  }
  if (instanceof(ov, LispList) == true)
    return new LispAtom(((RLispList)ov)->toString());
  if (instanceof(ov, LispArray) == true)
    return new LispAtom(((RLispArray)ov)->toString());
  return new LispAtom(RString("Unknown"));
}

RLispVar 
lisp_not(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2) 
    THROW2(LispException, env, "not needs 1 argument");
  RLispVar test = args->cdr()->car();
  if (test == Nil)
    return &env->t();
  //RString tstr = test->toCode();
  if (instanceof(test, LispAtom) == true) {
    if (RLispAtom(test)->val().getBoolVar() == false)
      return &env->t();
  }
  return Nil;
}

RLispVar 
lisp_and(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  args = args->cdr();
  while (args != Nil) 
  {
    if (isTrue(env->eval(args->car())) == false)
      return Nil;
    args = args->cdr();
  }
  return &env->t();
}

RLispVar 
lisp_or(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  args = args->cdr();
  while (args != Nil) {
    if (isTrue(env->eval(args->car())) == true)
      return &env->t();
    args = args->cdr();
  }
  return Nil;
}

RLispVar 
lisp_eq(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 3) 
    THROW2(LispException, env, "eq needs 2 argument");
  args = args->cdr();
  RLispVar a1 = args->car();
  RLispVar a2 = args->cdr()->car();
  if (a1 == a2) //### correct??
    return &env->t();
  return Nil;
}

RLispVar 
lisp_eql(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 3) 
    THROW2(LispException, env, "eql needs 2 argument");
  args = args->cdr();
  RLispVar a1 = args->car();
  RLispVar a2 = args->cdr()->car();
  if (a1 == Nil && a2 == Nil)
    return &env->t();
  if (a1 == Nil || a2 == Nil)
    return Nil;
  if (instanceof(a1, LispAtom) == true && instanceof(a2, LispAtom) == true) {
    if (RLispAtom(a1)->val().equal(RLispAtom(a2)->val()).getBoolVar() == true)
      return &env->t();
    return Nil;  
  }
  if (a1->toString()->compareTo(a2->toString()) == 0)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_number_equal(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  if (args->length() != 3) 
    THROW2(LispException, env, "= needs 2 argument");
  args = args->cdr();
  RLispVar a1 = args->car();
  RLispVar a2 = args->cdr()->car();
  if (instanceof(a1, LispAtom) == false || instanceof(a2, LispAtom) == false)
    THROW2(LispException, env, "= needs 2 atoms");
  if (RLispAtom(a1)->val().equal(RLispAtom(a2)->val()).getBoolVar() == true)
    return &env->t();
  return Nil;
}



RLispVar 
lisp_lambda(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "lambda needs 1 or more arguments");
  RLispFunction func = new LispFunction(args);
  return &func;
}

/** 
  args[0] == "defun"
  args[1] == funcname
  args[2] == args
  args[3 - n] == to eval
*/
RLispVar 
lisp_defun(IN(RLispEnvironment) env, IN(RLispList) args)
{

  if (args->length() < 4) 
    THROW2(LispException, env, "defun needs 3 or more arguments");
  RLispFunction func = new LispFunction(args);
  env->registerDefun(func);
  return args->get(1);
}

RLispVar 
lisp_defmacro(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispFunction lf = new LispFunction(args);
  env->registerDefun(lf);
  return &lf;
}



RLispVar 
lisp_progn(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispList lvar = args->cdr();
  RLispVar retvar;
  while (lvar != Nil) {
    //StackHolder<RLispVar> __evalStackHolder(env->_evalStack, lvar->car());
    retvar = env->eval(lvar->car());
    if (env->returnNow() == true) 
      return retvar;
    if (env->exitNow() == true)
      return Nil;
    lvar = lvar->cdr();
  }
  return retvar;
}

RLispVar 
lisp_apply(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispList lvar = args->cdr();
  RLispVar functoApply = lvar->car();
  RLispList argsforfunc = (RLispList)lvar->cdr()->car();
  RLispList largs = new LispList();
  largs->append(new LispSymbol(functoApply->toString()));
  largs->setCdr(argsforfunc);
  RFunction func = env->lookupFunction(functoApply->toString());
  if (func == Nil)
    THROW2(LispException, env, "cannot find function: [" + functoApply->toString() + "] in [" + args->toCode() + "]");
  return func->eval(env, largs);
}

RLispVar 
lisp_acdk_lisp_include(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RString fname = args->cdr()->car()->toString();
  if (File(fname).isAbsolute() == false) {
    RString cparentdir;
    if (env->_modulStack.empty() == false) {
      RFile lmod = env->_modulStack.top();
      cparentdir = lmod->getParent();
      File f(cparentdir, fname);
      if (f.exists() == false)
        cparentdir = acdk::io::File::getCWD();    
    } else {
      cparentdir = acdk::io::File::getCWD();
    }
    File f(cparentdir, fname);
    if (f.exists() == true)
      fname = f.getCanonicalPath();
    else {
      RLispVar acdkhome = env->lookupVar("ACDK_TOOLS_HOME");
      if (acdkhome == Nil)
        acdkhome = env->lookupVar("ACDKHOME");
      if (acdkhome == Nil)
        THROW2(LispException, env, "Environment ACDKHOME or ACDK_TOOLS_HOME is not set");
      RString lsphome = acdkhome->toString() + "/cfg/lib/acdk/lisp";
      File sf(lsphome, fname);
      RString fqname = sf.getCanonicalPath();
      if (sf.exists() == true)
        fname = fqname;
      else
        THROW2(LispException, env, "Cannot find lisp-File: [" + fname + "]");
    }
  }
  RString fqname = fname; 
  if (env->includes()->contains((RObject)fqname) == true) 
    return Nil;
  //env->out->println(RString("include ") + fqname);
  env->includes()->add((RObject)fqname);
  try {
    env->load(fname);
  } catch (::acdk::io::RIOException ioex) {
    THROW2(LispException, env, "IO-Error in include: [" + ioex->getMessage() + "]");
  }
  return Nil;
}

RLispVar 
lisp_zerop(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "zerop needs 1 or more arguments");
  if (args->cdr() == Nil || args->cdr()->car() == Nil || args->cdr()->car() == Nil) // ###### double check for cdr()->car()????
    return &env->t();
  return Nil;
}

RLispVar 
lisp_truep(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "truep needs 1 or more arguments");
  if (args->cdr() == Nil || args->cdr()->car() == Nil)
    return Nil;
  if (args->cdr()->car() == env->t())
    return &env->t();
  return Nil;
}

RLispVar 
lisp_lisp(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "lisp needs 1 or more arguments");
  if (args->cdr() == Nil || args->cdr()->car() == Nil || args->cdr()->car() == Nil)
    return Nil;
  if (instanceof(args->cdr()->car(), LispList) == true)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_atomp(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "atomp needs 1 or more arguments");
  if (args->cdr() == Nil || args->cdr()->car() == Nil || args->cdr()->car() == Nil)
    return Nil;
  if (instanceof(args->cdr()->car(), LispAtom) == true)
    return &env->t();
  return Nil;
}

RLispVar 
lisp_symbolp(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "atomp needs 1 or more arguments");
  if (args->cdr() == Nil || args->cdr()->car() == Nil || args->cdr()->car() == Nil)
    return Nil;
  if (instanceof(args->cdr()->car(), LispSymbol) == true)
    return &env->t();
  return Nil;
}

RLispVar
lisp_trace(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) {
    if (env->trace() == true)
      return &env->t();
    else
      return Nil;
  }
  RLispVar farg = args->cdr()->car();
  if (instanceof(farg, LispAtom) == true) {
    if (RLispAtom(farg)->val().getBoolVar() == true)
      env->trace(true);
    else
      env->trace(false);
  }
  if (instanceof(farg, LispSymbol) == true) {
    bool setvar = false;
    if (args->cdr()->cdr() && args->cdr()->cdr()->car() && instanceof(args->cdr()->cdr()->car(), LispAtom))
      setvar = RLispAtom(args->cdr()->cdr()->car())->val().getBoolVar();
    env->trace(farg->toString(), setvar);
    return &env->t();
  }
  return &env->t();
}


RLispVar
lisp_explore(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2) 
    THROW2(LispException, env, "explore need one object instance");
  RLispVar vobj = args->cdr()->car();
  RObject obj = vobj->getObject();
  RClass cls = obj->getClass();
  RMethodArray methods =  cls->getMethods();
  StringBuffer sb;
  sb.append("Methods of Class " + cls->getName() + "\n");
  for (int i = 0; i < methods->length(); i++) {
    RMethod method = methods[i];
    if (method != Nil)
      sb.append("  " + method->toString() + "\n");
  }
  return  new LispAtom(ScriptVar((RObject)sb.toString()));
  //return Nil;
}

RLispVar 
lisp_do(IN(RLispEnvironment) env, IN(RLispList) args)
{
  return Nil;
}

RLispVar 
lisp_dolist(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2)
    THROW2(LispException, env, "dolist need 2 or more arguments");
  // dolist (element list erg) body1 bodyn)
  RLispList a = args->cdr();
  RLispList dargs = RLispList(a->car());
  RLispSymbol element = RLispSymbol(dargs->car());
  RLispVar lvar = dargs->cdr()->car();
  lvar = env->eval(lvar);
  
  if (lvar != Nil && instanceof(lvar, LispList) == false)
    THROW2(LispException, env, "Variable is not a List: " + lvar->toCode());
  RLispList list = RLispList(lvar);
  RLispVar erg = Nil;
  if (dargs->cdr()->cdr() != Nil) {
    erg = dargs->cdr()->cdr()->car();
  }
  RLispList body = a->cdr();
  while (list != Nil) {
    env->bindLocal(element->toString(), list->car(), true);
    RLispList e = body;
    while (e != Nil) {
      RLispVar terg = env->eval(e->car());
      if (env->returnNow() == true)
        return terg;
      if (env->exitNow() == true)
        return Nil;
      e = e->cdr();
    }
    list = list->cdr();
  }
  return Nil;
}

RLispVar 
lisp_dp(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() != 2)
    THROW2(LispException, env, "dp need 1 or more arguments");
  if (args->length() < 2 || 
      instanceof(args->cdr()->car(), LispAtom) == false || 
      RLispAtom(args->cdr()->car())->val().type != ScriptVar::ObjectType) 
    THROW2(LispException, env, "explore need one object instance");
  RObject obj = RLispAtom(args->cdr()->car())->val().getObjectVar();
  return new LispAtom(ScriptVar((RObject)obj->toString()));
}

RLispVar 
lisp_create_array(IN(RLispEnvironment) env, IN(RLispList) arguments)
{
  RLispList args = arguments;
  args = args->cdr();
  if (args->car() == Nil || instanceof(args->car(), LispAtom) == false)
    THROW2(LispException, env, "create-array needs element count as argument");
  int i = RLispAtom(args->car())->val().getIntVar();
  RObject obj = new ObjectArray(i);
  return new LispAtom(ScriptVar(obj));
}

bool
matchThrowType(IN(RLispVar) thrown, IN(RThrowable) ex)
{
  if (thrown->toString()->equals((RString)"...")  == true) 
    return true;
  if (instanceof(thrown, LispSymbol) == true) {
    RString clazzname = toClazzName(thrown->toString());
    RClass cls = Class::forName(clazzname);
    if (cls == Nil)
      return false;
    return ex->getClass()->isAssignableFrom(cls);
  }
  return false;
}

RLispVar 
lisp_try(IN(RLispEnvironment) env, IN(RLispList) args)
{
  /*(try 
      (
        *(tryblock)
      )
      (catch (acdk.lang.Throwable ex)
          *(handler)
      )
     )
    */
  RLispList l = args->cdr();
  RLispList tryblocks = (RLispList)l->car();
  //RString tstr = tryblocks->toCode();
  l = l->cdr();
  //tstr = l->toCode();
  RLispList catchlist = l;
  RLispVar erg;
  try {
    StackVarHolder _stack(env, true);
    //tstr = tryblocks->toCode();
    while (tryblocks != Nil) {
      erg = env->eval(tryblocks->car());
      tryblocks = tryblocks->cdr();
      if (env->returnNow() == true)
        return erg;
      if (env->exitNow() == true)
        return Nil;
    }
  } catch (RThrowable ex) {
    bool foundmatchedcatched = false;
    while (catchlist != Nil && foundmatchedcatched == false) {
      RLispList b = (RLispList)catchlist->car();
      //tstr = b->car()->toString();
      if (b->car()->toString()->equals("catch") == false)
        THROW2(LispException, env, "catch expected");
      b = b->cdr();
      RLispList th = (RLispList)b->car();
      //tstr = th->toCode();
      RLispVar throwtype = th->car();
      b = b->cdr();
      if (matchThrowType(throwtype, ex)  == true) {
        StackVarHolder _stack(env, true);
        if (th->cdr() != Nil && th->cdr()->car() != Nil) {
          RLispVar e = new LispAtom(ScriptVar(ex));
          env->bindLocal(th->cdr()->car()->toString(), e, true);
        }
        //tstr = b->toCode();
        foundmatchedcatched = true;
        while (b != Nil) {
          //tstr = b->car()->toCode();
          erg = env->eval(b->car());
          if (env->returnNow() == true)
            return erg;
          if (env->exitNow() == true)
            return Nil;
          
          b = b->cdr();
        }
      } 
      catchlist = catchlist->cdr();
    }
    if (foundmatchedcatched == false)
      throw;
  }
  return erg;
}

RLispVar 
lisp_throw(IN(RLispEnvironment) env, IN(RLispList) args)
{
  RLispVar throwtype = lisp_new(env, args);
  if (throwtype == Nil)
    return Nil;
  RThrowable ex = (RThrowable) ((RLispAtom)throwtype)->val().getObjectVar();
  throw ex;
  return Nil;
}


} // namespace lisp
} // namespace acdk

