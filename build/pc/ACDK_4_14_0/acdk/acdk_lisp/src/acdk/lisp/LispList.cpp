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


#include "LispList.h"
#include "LispAtom.h"

namespace acdk {
namespace lisp {

//tatic 
RLispVar LispList::_nilVar;




//virtual 
RObject 
LispList::clone(sys::Allocator* alc)
{
  RLispList nl = new LispList();
  if (car() != Nil)
    nl->setCar(car());
  RLispList cl = cdr();
  while (cl != Nil) {
    nl->append(cl->car());
    cl = cl->cdr();
  }
  return &nl;
}


RLispList 
LispList::append(IN(RLispVar) var)
{
  RLispList l = this;
  while (l->cdr() != Nil)
    l = l->cdr();
//  if ((l == this) && (l->car() == Nil))
//    l->setCar(var);
//  else
    l->setCdr(new LispList(var));
  return this;
}

RLispList 
LispList::unshift(IN(RLispVar) var)
{
  RLispList ll =  new LispList(var);
  ll->setCdr(this);
  return ll;
}

RLispList 
LispList::unshift(IN(RObject) var)
{
  if (var != Nil && instanceof(var, LispVar))
    return unshift(RLispVar(var));
  else
    return unshift(RLispVar(new LispAtom(ScriptVar(var))));
}

RLispList 
LispList::push(IN(RObject) var) 
{ 
  if (var != Nil && instanceof(var, LispVar))
    return push(RLispVar(var));
  RLispVar lv = new LispAtom(ScriptVar(var));
  return push(lv); 
}

RLispVar 
LispList::pop() 
{
  RLispList cl = this;
  RLispList pcl = Nil;
  while (cl != Nil) {
    if (cl->cdr() == Nil)
      break;
    pcl = cl;
    cl = cl->cdr();
  }
  if (cl == Nil)
    return Nil;
  RLispVar l = cl->car();
  if (pcl == Nil) {
    _cdr = Nil;
    _car = Nil;
  } else
    pcl->setCdr(Nil);
  return l;
}

RString 
LispList::toString()
{
  StringBuffer buf(200);
  RLispList l = this;
  do {
    RString ts = l->_car == Nil ? RString("NIL") : l->_car->toString();
    buf.append(ts);
    if (l->_cdr == Nil)
      break;
    buf.append(" ");
    l = l->_cdr;
  } while (true);
  return buf.toString();
}

RString 
LispList::toCode()
{
  StringBuffer buf(200);
  RLispList l = this;
  buf.append("(");
  do {
    RString tstr = l->_car == Nil ? RString("NIL") : l->_car->toCode();
    buf.append(tstr);
    if (l->_cdr == Nil)
      break;
    buf.append(" ");
    l = l->_cdr;
  } while (true);
  buf.append(")");
  return buf.toString();
}

int 
LispList::length() 
{ 
  RLispList l = this;
  int i = 1;
  while (true) {
    if (l->_cdr == Nil)
      return i;
    i++;
    l = l->_cdr;
  }
  return i;
}

RLispVar 
LispList::get(int i)
{
  RLispList l = this;
  while (i-- > 0) {
    if (l->_cdr == Nil)
      THROW1(Exception, "IndexOutBound");
    l = l->_cdr;
  }
  return l->_car;
}

void 
LispList::set(int i, IN(RLispVar) var)
{
  //System::err->println(RString("LE::set: setting element ") + i + " to " + (var == Nil)? "NIL":var->toString());
  RLispList l = this;
  while (i-- > 0) {
    if (l->_cdr == Nil)
      l->_cdr = new LispList();
    l = l->_cdr;
  }
  l->_car = var;
}




} // namespace lisp
} // namespace acdk



