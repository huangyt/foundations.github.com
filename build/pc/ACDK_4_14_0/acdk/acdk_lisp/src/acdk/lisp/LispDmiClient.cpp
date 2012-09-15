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


#include "LispDmiClient.h"
#include "LispList.h"
#include "LispEnvironment.h"

namespace acdk {
namespace lisp {

using ::acdk::lang::dmi::ClazzInfo;
using ::acdk::lang::dmi::ScriptVar;

//virtual 
int 
LispDmiClient::typeDistance(const ScriptVar& arg, const ClazzInfo* toType)
{
  int ret = Super::typeDistance(arg, toType);
  if (ret == 0)
    return ret;

  return ret;
}

int 
LispDmiClient::typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
{
  int ret = Super::typeDistance(fromType, toType);
  if (ret != -1)
    return ret;
  if (toType->isArray() && fromType == LispList::clazzInfo())
  {
    return 262;
  }
  return ret;
}

bool convertArg(IN(RLispEnvironment) env, ScriptVar& target, IN(RLispVar) source);

//virtual 
void 
LispDmiClient::castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType)
{
  RLispEnvironment lenv = LispEnvironment::lenv();
  if (typeDistance(value, toType) == 262 && toType->isArray() == true)
  {
    RLispList ll = (RLispList)value.getObjectVar();
    RObject oa;
    if (toType->getElementClazzInfo()->array_creator != 0)
      oa = toType->getElementClazzInfo()->array_creator(ll->length());
    else
      oa = new ObjectArray(ll->length());
    for (int i = 0; ll != Nil; ++i)
    {
      ScriptVar tv;
      convertArg(lenv, tv, ll->car());
      oa->invoke("set", i, tv.getObjectVar());
      ll = ll->cdr();
    }
    value = &oa;
  } else
    Super::castTo(value, toType);
}

} // namespace lisp
} // namespace acdk


