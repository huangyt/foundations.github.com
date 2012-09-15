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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/AObjectImpl.cpp,v 1.10 2005/02/05 10:45:39 kommer Exp $


#include "AObjectImpl.h"

namespace acdkx {
namespace arb {


ObjectID::ObjectID(IN(RString) str)
{
  RString rest = str;

  protokoll = rest->substr(0, rest->indexOf(':'));
  rest = rest->substr(rest->indexOf(':') + 1);
  network = rest->substr(0, rest->indexOf(':'));
  rest = rest->substr(rest->indexOf(':') + 1);
  port = Integer::parseInt(rest->substr(0, rest->indexOf(':')));
  rest = rest->substr(rest->indexOf(':') + 1);
  pid = Integer::parseInt(rest->substr(0, rest->indexOf(':')));
  rest = rest->substr(rest->indexOf(':') + 1);
  classname = rest->substr(0, rest->indexOf(':'));
  rest = rest->substr(rest->indexOf(':') + 1);
  /*if (pid == Process::getProcessId())
    object = (Object*)Integer::parseInt(rest);
  else*/
    object = rest;

}

AObjectImpl::AObjectImpl()
  : Object(),
    _localImpl(Nil),
    _objID(Nil)
{
}

AObjectImpl::AObjectImpl(IN(RObjectID) objid)
  : Object(),
    _localImpl(Nil),
    _objID(objid)
{
}

void 
AObjectImpl::setDelegater(IN(RADelegate) del) 
{ 
  _delegate = del; 
}


//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
AObjectImpl::lookupMethod(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, const ::acdk::lang::dmi::ClazzInfo* clazz)
{
  const ::acdk::lang::dmi::ClazzInfo* ci = clazz;
  int i = 0;
  while (ci->methods[i]) {
    if (strcmp(ci->methods[i]->name, fname) == 0)
      return ci->methods[i];
    i++;
  }
  const ::acdk::lang::dmi::ClazzMethodInfo* merg = 0;
  for (i = 0; ci->interfaces[i]; i++) {
    merg = lookupMethod(fname, args, ci->interfaces[i]->type);
    if (merg != 0)
      return merg;
  }
  return 0;
}

} // namespace arb 
} // namespace acdkx 



