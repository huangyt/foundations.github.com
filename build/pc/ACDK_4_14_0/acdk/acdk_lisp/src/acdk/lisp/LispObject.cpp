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




#include "LispObject.h"
#include "LispException.h"
#include "LispEnvironment.h"
#include <acdk/lang/ParamsMismatchException.h>

namespace acdk {
namespace lisp {

//static
RLispList LispObject::_definition;

//virtual
RLispList
LispObject::getDefinition() 
{ 
  if (_definition != Nil)
    return _definition;
  _definition =  LispEnvironment::lenv()->parseToList("(self methodname &rest args)"); 
  return _definition;
}

RLispVar acdk2lisp(const ScriptVar& sv)
{
  if (sv.isObjectType() == true)
  {
    RObject obj = sv.getObjectVar();
    if (instanceof(obj, RLispVar) == true)
      return RLispVar(obj);
  }
  return new LispAtom(sv);
}


void 
LispObject::initObject(IN(RString) classname, IN(NamedArgs) args)
{
  RLispEnvironment env = LispEnvironment::lenv();
  RLispVar lvar = env->lookupVar(classname);

  if (lvar == Nil && instanceof(lvar, LispClass) == true)
    THROW1(ClassNotFoundException, "Cannot find LispClass: " + classname);
  _class = RLispClass(lvar);

  initObject(env, _class);

  for (int i = 0; i < args.size(); ++i)
  {
    if (setSlotByInitArg(env, args[i].name(), acdk2lisp(args[i].value())) == false)
      THROW1(ParamsMismatchException, "Cannot not initialize Class " + classname + " with argument " + args[i].name());
  }
}

LispObject::LispObject(IN(RString) classname, IN(NamedArgs) args)
: _slots(new ::acdk::util::HashMap())
{
  initObject(classname, args);
}

  
//foreign virtual 
RString 
LispObject::toString() 
{ 
  return "LispObject"; 
}

//foreign virtual 
RString 
LispObject::toCode() 
{ 
  StringBuffer sb("'(");
  ::acdk::util::RIterator it = _slots->keySet()->iterator();
  while (it->hasNext() == true)
  {
    RObject obj = it->next();
    sb << "('" << obj->toString() 
       << " ";
    RObject vobj = _slots->get(obj);
    if (vobj == Nil)
      sb << "Nil";
    else
      sb << RLispVar(vobj)->toCode();
    sb << ")";
  }
  sb << ")";
  return sb.toString();
}

void 
LispObject::initObject(IN(RLispEnvironment) env, IN(RLispClass) cls)
{
  int i;
  for (i = 0; i < cls->supers()->length(); ++i)
  {
    initObject(env, cls->supers()[i]);
  }

  RLispSlotArray slots = cls->slots();
  for (i = 0; i < slots->length(); ++i)
  {
    RLispSlot slot = slots[i];
    if (slot->isStatic == true)
      continue;
    RLispVar nval = slot->initform;
    _slots->put(&slot->name, &nval);
  }
}

bool 
LispObject::setSlotByInitArg(IN(RLispEnvironment) env, IN(RLispClass) cls, IN(RString) initarg, IN(RLispVar) val)
{
  int i;
  RLispSlotArray slots = cls->slots();
  for (i = 0; i < slots->length(); ++i)
  {
    RLispSlot slot = slots[i];
    if (slot->isStatic == true)
      continue;

    if (slot->initarg != Nil && slot->initarg->equals(initarg) == true)
    {
      RLispVar nval = val;
      _slots->put(&slot->name, &nval);
      return true;
    }
  }
  for (i = 0; i < cls->supers()->length(); ++i)
  {
    if (setSlotByInitArg(env, cls->supers()[i], initarg, val) == true)
      return true;
  }
  return false;
} 

// (self (qoute member) args)
//virtual 
RLispVar 
LispObject::eval(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2)
    THROW2(LispException, env, "LispObject member invocation/access needs at least 1 argument: " + args->toCode());
  
  RLispList targs = args;
  
  RLispVar self = targs->car();
  targs = targs->cdr();
  //RString tstr = targs->toCode();
  RString membern = env->eval(targs->car())->toString();

  targs = targs->cdr();
  RLispVar member = getSlot(membern);
  if (instanceof(member, Function) == true)
  {
    RLispList nargs = new LispList(self, targs);
    RLispList nm = new LispList(member, nargs);

    return env->_eval(nm);
  } 
  if (targs == Nil || targs->length() == 0)  // getter
  {
    return member;
  } 
  else if (targs->length() == 1) // setter
  {
    setSlot(membern, env->eval(targs->car()));
    return targs->car();
  }
  THROW2(LispException, env, "LispObject member access needs 1 (getter) or 2 (setter) arguments" + args->toCode());
  return Nil;
}

RLispVar 
lisp_make_instance(IN(RLispEnvironment) env, IN(RLispList) args)
{
  if (args->length() < 2)
    THROW2(LispException, env, "make-instance needs at least 1 argument: " + args->toCode());
  
  RLispList targs = args->cdr();
  
  RLispVar tclvar = env->eval(targs->car());
  if (instanceof(tclvar, LispClass) == false)
    tclvar = env->lookupVar(tclvar->toString());
  if (tclvar == Nil || instanceof(tclvar, LispClass) == false)
    THROW2(LispException, env, "Cannot find LispClass with name: " + tclvar->toCode());
  RLispClass lclass = RLispClass(tclvar);
  RLispObject lobj = new LispObject(env, lclass);
  targs = targs->cdr();
  while (targs != Nil && targs->car() != Nil)
  {
    RString n = targs->car()->toString();
    targs = targs->cdr();
    RLispVar val = targs->car();
    if (lobj->setSlotByInitArg(env, n, val) == false)
      THROW2(LispException, env, "Cannot not initialize Class " + tclvar->toCode() + " with argument " + n);
    targs = targs->cdr();
  }
  return &lobj;
}



RLispList 
toLispArgs(::acdk::lang::dmi::ScriptVarArray& args, 
                                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs)
{
  int nonnamedargsnum = args.size();
  if (namedArgs != Nil)
    nonnamedargsnum =- namedArgs->length();
  
  RLispList first;
  RLispList ll;
  int i;
  for (i = 0; i < nonnamedargsnum; ++i)
  {

    RLispList tl = new LispList(acdk2lisp(args[i]));
    if (first == Nil)
      first = tl;
    if (ll != Nil)
      ll->setCdr(tl);
    ll = tl;
  }
  for (; i < args.size(); ++i)
  {
    RLispList tl = new LispList(new LispSymbol(namedArgs[i - nonnamedargsnum]));
    if (first == Nil)
      first = tl;
    if (ll != Nil)
      ll->setCdr(tl);
    ll = tl;
    tl = new LispList(acdk2lisp(args[i]));
    ll->setCdr(tl);
    ll = tl;
  }
  return first;
}


//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
LispObject::dynamic_dispatch(::acdk::lang::Object* This_, 
                                                         IN(RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  RLispEnvironment env = LispEnvironment::lenv();
  RLispObject This = dynamic_cast<LispObject*>(This_);
  //RLispVar slot = getSlot(fname);
  //if (slot == Nil)
//    THROW1(MethodNotFoundException, "Member cannot be found: " + getLispClass()->getName());
  RLispList largs = toLispArgs(args, dc, namedArgs);
  RLispList func = new LispList(new LispSymbol("quote"), new LispList(new LispSymbol(&fname)));
  RLispList fn = new LispList(&This, new LispList(&func, largs));
  //RString tstr = fn->toCode();
  RLispVar erg = This->eval(env, fn);
  if (instanceof(erg, LispAtom) == true)
    ret = RLispAtom(erg)->val();
  else
    ret = &erg;
  return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
}


// static
const ::acdk::lang::dmi::ClazzMethodInfo* 
LispObject::static_dispatch(IN(RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  return 0;
}

/*
//virtual 
::acdk::lang::dmi::ScriptVar 
LispObject::getMember(const char* fieldname, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested )
{
  return ScriptVar();
}
  
//virtual 
void 
LispObject::setMember(const char* fieldname, const ::acdk::lang::dmi::ScriptVar& newval, ::acdk::lang::dmi::DmiClient& dc, int flags)
{

}
*/

} // namespace lisp
} // namespace acdk



