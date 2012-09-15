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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Boolean.cpp,v 1.11 2005/02/05 10:44:55 kommer Exp $


#include <acdk.h>

#include "System.h"
#include "Boolean.h"

namespace acdk {
namespace lang {

//static 
RBoolean  Boolean::_falseValue;
//static 
RBoolean  Boolean::_trueValue;
//static 
RBoolean 
Boolean::getFALSE()
{
  if (_falseValue == Nil) {
    _falseValue = new Boolean(false);
    System::registerStaticReference(_falseValue);
  }
  return _falseValue;
}

//static 
RBoolean 
Boolean::getTRUE()
{
  if (_trueValue == Nil) {
    _trueValue = new Boolean(true);
    System::registerStaticReference(_trueValue);
  }
  return _trueValue;
}

Boolean::Boolean(IN(RString) val)
: Object(),
  value(false)
{
  value = (val != Nil && val->equalsIgnoreCase(new String("true")) == true);
}


//virtual 
bool 
Boolean::equals(IN(RObject) obj)
{
  if (instanceof(obj, Boolean) == false)
    return false;
  return RBoolean(obj)->booleanValue() == booleanValue();
}

//virtual 
RString 
Boolean::toString()
{
  return new String(value ? "true" : "false");
}

//virtual 
int 
Boolean::hashCode()
{
  return value ? 1 : 0;
}

//static 
bool 
Boolean::getBoolean(IN(RString) name)
{
  Boolean b(name);
  return b.booleanValue();
}

//static 
RBoolean 
Boolean::valueOf(IN(RString) s)
{
  return new Boolean(s);
}


//static 
RClass 
Boolean::getTYPE() 
{ 
  return Class::getSingeltonClass(dmi::ClazzInfo::getBoolClazz());
}
 
} // lang
} // acdk
