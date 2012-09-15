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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Enumeration.cpp,v 1.6 2005/03/06 11:54:22 kommer Exp $

#include <acdk.h>
#include "Enumeration.h"

namespace acdk {
namespace lang {
namespace reflect {


//static 
REnumeration 
Enumeration::getEnumeration(IN(RString) enumName, IN(RString) namesp)
{
  const acdk::lang::dmi::ClazzEnumInfo* ei = acdk::lang::dmi::ClazzEnumInfo::findEnum(enumName, namesp);
  if (ei == 0)
    return Nil;
  return new Enumeration(ei);
}

//static 
REnumerationValue 
Enumeration::getEnumerationValue(IN(RString) enumName, IN(RString) namesp)
{
  const acdk::lang::dmi::ClazzEnumInfo* ei = 0;
  const acdk::lang::dmi::ClazzEnumValueInfo* ev = acdk::lang::dmi::ClazzEnumInfo::findEnumValue(enumName, namesp, &ei);
  if (ev == 0)
    return Nil;
  return new EnumerationValue(ei, ev);
}


//static 
REnumerationArray 
Enumeration::getEnumerations()
{
  REnumerationArray ea = new EnumerationArray(0);
  for (const acdk::lang::dmi::ClazzEnumInfo* ei = acdk::lang::dmi::ClazzEnumInfo::getRoot();
       ei != 0; ei = ei->_next)
  {
    ea->append(new Enumeration(ei));
  }
  return ea;
}

RString 
Enumeration::getName()
{
  return _enumInfo->name;
}

bool 
Enumeration::equals(IN(RObject) obj)
{
  if (instanceof(obj, Enumeration) == false)
    return false;
  REnumeration eif(obj);
  return eif->_enumInfo == _enumInfo;
}

RString 
Enumeration::toIndentifier()
{
  return getName();
}

REnumerationValueArray 
Enumeration::getValues()
{
  REnumerationValueArray eva = new EnumerationValueArray(0);
  for (int i = 0; _enumInfo->values[i] != 0; ++i)
  {
    eva->append(new EnumerationValue(_enumInfo, _enumInfo->values[i]));
  }
  return eva;
}

RString 
Enumeration::getEnumNameForValue(int value)
{
  for (int i = 0; _enumInfo->values[i] != 0; ++i)
  {
    if (_enumInfo->values[i]->value == value)
      return _enumInfo->values[i]->name;
  }
  return Nil;
}

//static 
RString 
Enumeration::getEnumNameForValue(IN(RString) enumName, int value)
{
  const acdk::lang::dmi::MetaInfo* mi = acdk::lang::dmi::MetaInfo::findMetaInfo(enumName);
  if (mi == 0)
    return Nil;
  if (mi->isEnumInfo() == false)
    return Nil;
  return Enumeration(reinterpret_cast<const acdk::lang::dmi::ClazzEnumInfo*>(mi)).getEnumNameForValue(value);
}

} // reflect
} // lang
} // acdk



