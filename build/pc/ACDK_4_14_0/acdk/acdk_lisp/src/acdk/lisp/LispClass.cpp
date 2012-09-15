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




#include "LispClass.h"
#include "LispException.h"
#include "LispEnvironment.h"

namespace acdk {
namespace lisp {

RString 
LispSlot::toCode()
{
  if (initarg == Nil && isStatic == false && initform == Nil)
    return name;
  StringBuffer sb("(");
  sb << name;
  if (initarg != Nil)
    sb << " :initarg " << initarg;
  if (initform != Nil)
    sb << " :initform " << initform->toCode();
  if (isStatic == true)
    sb << " :allocation :class";

  sb << ")";
  return sb.toString();
}
  
  //virtual 
RString 
LispClass::toString()
{
  return "LispClass " + _className;
}

//virtual 
RString 
LispClass::toCode()
{
  StringBuffer sb("(defclass ");
  sb << _className << " (";
  int i;
  for (i = 0; i < _superClasses->length(); ++i)
  {
    if (i != 0)
      sb << " ";
    sb << _superClasses[i]->className();
  }
  sb << ") (";
  
  for (i = 0; i < _slots->length(); ++i)
  {
    if (i != 0)
      sb << " ";
    sb << _slots[i]->toCode();
  }
  sb << ") )";
  return sb.toString();
}

//virtual 
RObject 
LispClass::clone(sys::Allocator* alc)
{
  RLispClass cl = new (alc) LispClass(_className);
  cl->_superClasses = (RLispClassArray)_superClasses->clone(alc);// LispClassArray(_superClasses);
  cl->_slots = (RLispSlotArray)_superClasses->clone(alc);
  return &cl;
}

RLispVar 
lisp_defclass(IN(RLispEnvironment) env, IN(RLispList) args)
{
  int len = args->length();
  if (args->length() != 4)
    THROW2(LispException, env, "defclass needs 2 arguments: " + args->toCode());
  RLispList targs = args->cdr();
  RString cname = targs->car()->toString();
  targs = targs->cdr();
  RLispList supers = (RLispList)targs->car();
  targs = targs->cdr();
  RLispList slots = (RLispList)targs->car();
  RLispClass lclass = new LispClass(cname);
  
  RLispList l = supers;
  while (l != Nil && l->car() != Nil) 
  {
    lclass->addClass(RLispClass(env->lookupVar(l->car()->toString())));
    l = l->cdr();
  }
  
  l = slots;
  while (l != Nil) 
  {
    if (instanceof(l->car(), LispSymbol) == true)
    {
      lclass->addSlot(new LispSlot(l->car()->toString()));
    } 
    else if (instanceof(l->car(), LispList) == true) 
    {
      RLispList sl = (RLispList)l->car();
      RLispSlot slot;
      while (sl != Nil && sl->car() != Nil)
      {
        if (slot == Nil)
        {
          slot = new LispSlot(sl->car()->toString());
        } 
        else 
        {
          RString t = sl->car()->toString();
          if (t->equals(":initarg") == true)
          {
            sl = sl->cdr();
            slot->initarg = sl->car()->toString();
          } 
          else if (t->equals(":initform") == true)
          {
            sl = sl->cdr();
            RLispVar lvar = sl->car();
            slot->initform = env->eval(lvar);
          }
          else if (t->equals(":allocation") == true)
          {
            sl = sl->cdr();
            RString t = sl->car()->toString();
            if (t->equals(":class") == true)
              slot->isStatic = true;
            else
              slot->isStatic = false;
          }
        }
        sl = sl->cdr();
      }
      lclass->addSlot(slot);
    }
    l = l->cdr();
  }
  //RString tcode = lclass->toCode();

  env->bindGlobal(lclass->className(), &lclass);
  return &lclass;
}




} // namespace lisp
} // namespace acdk



