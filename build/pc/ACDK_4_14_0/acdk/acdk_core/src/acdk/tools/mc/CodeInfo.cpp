
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



#include "CodeInfo.h"
#include "SetDispatchAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

USING_CLASS(::acdk::io::, PrintWriter);


void 
CodeInfo::addCodeAttribute(IN(RCodeAttribute) ca)
{
  RCodeAttributeArray caa = getCodeAttributes();
  caa->append(ca);
}

void 
CodeInfo::addCode(IN(RString) code, CodeWhere where)
{
  if (where != ModuleInclude)
  {
    _props->appendObjectList("_code", new CodeInsertion(code, where), 0);
    return;
  }
  RObjectArray oa = (RObjectArray)_props->getObjectVal("_code", false);
  if (oa == Nil)
  {
    _props->appendObjectList("_code", new CodeInsertion(code, where), 0);
    return;
  }
  for (int i = 0; i < oa->length(); ++i)
  {
    RCodeInsertion ci(oa[i]);
    if (ci->insertPoint == ModuleInclude)
    {
      if (ci->code->equals(code) == true)
        return;
    }
  }
  _props->appendObjectList("_code", new CodeInsertion(code, where), 0);
}

void 
CodeInfo::writeCode(IN(RPrintWriter) out, CodeWhere where)
{
  RCodeInsertionArray cia = getCode();
  if (cia == Nil)
    return;
  for (int i = 0; i < cia->length(); ++i)
  {
    RCodeInsertion ci = cia[i];
    if (ci->insertPoint == where)
    {
      out->print(ci->code);
    }
  }
}

RCodeAttribute 
CodeInfo::getCodeAttribute(IN(RString) clsname)
{
  RCodeAttributeArray caa = (RCodeAttributeArray)(RObjectArray)_props->getObjectVal("_ca");
  if (caa == Nil)
    return Nil;
  for (int i = 0; i < caa->length(); ++i)
  {
    if (caa[i]->getClass()->getName()->equals(clsname) == true)
      return caa[i];
  }
  return Nil;
}

bool
CodeInfo::invokeCodeAttributes()
{
  bool berg = true;
  RCodeAttributeArray caa = getCodeAttributes();
  for (int i = 0; i < caa->length(); ++i)
  {
    berg &= caa[i]->apply(this);
  }
  return berg;
}

RString 
CodeInfo::getNamespace()
{
  int idx = name->lastIndexOf("::");
  if (idx == -1)
    return "";
  return name->substr(0, idx + 2);
}

RString 
CodeInfo::getBaseName()
{
  int idx = name->lastIndexOf("::");
  if (idx == -1)
    return name;
  return name->substr(idx + 2);
}

RString 
CodeInfo::getSuperName()
{
  int idx = name->lastIndexOf("::");
  if (idx == -1)
    return name;
  StringBuffer sb("ACDK_FQ_SUPER_QUALIFIER(");
  sb << name->substr(0, idx + 2) << ", " << name->substr(idx + 2) << ")";
  return sb.toString();
}

RString 
CodeInfo::getDispatchSignature(bool isStatic)
{
  RCodeAttributeArray ca = getCodeAttributes();
  if (ca == Nil)
    return Nil;
  for (int i = 0; i < ca->length(); ++i)
  {
    if (instanceof(ca[i], SetDispatchAttribute) == true)
    {
      RSetDispatchAttribute dsa = (RSetDispatchAttribute)ca[i];
      if (dsa->_staticCall == isStatic)
        return dsa->_functionSignature;
    }
  }
  return Nil;
}

} // namespace mc
} // namespace tools
} // namespace acdk


