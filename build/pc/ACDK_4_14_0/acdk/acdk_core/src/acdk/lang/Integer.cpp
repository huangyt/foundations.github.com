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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Integer.cpp,v 1.21 2005/03/24 16:58:00 kommer Exp $


#include <acdk.h>
#include <climits>
#include <stdio.h>

#include "NumberFormatException.h"
#include "Integer.h"
#include "Character.h"

namespace acdk {
namespace lang {

int Integer::MAX_VALUE = INT_MAX;
int Integer::MIN_VALUE = INT_MIN;
jlong Integer::serialVersionUID = JLONG_CONSTANT(1360826667806852920);

Integer::Integer(IN(RString) val) THROWS1(RNumberFormatException)
: Number(), value(0)
{
  value = parseInt(val, 0, true);
}


//virtual
RString
Integer::toString()
{
  return toString(value);
}

//static 
RString 
Integer::toString(int val, int radix)
{
  if (radix != 10)
    return String::_itoa(val, radix);
  char buffer[32];
  sprintf(buffer, "%i", val);
  return SCS(buffer);
}


//static 
int 
Integer::parseInt(IN(RString) s, int radix, bool decode)  THROWS1(RNumberFormatException)
{
  bool tryOnly = false;
  char typeChar = 'i';
  int ignoreaTrailing = 0;
  if (decode == true)
    return (int)decodeIntegerNumber(s, tryOnly, typeChar, ignoreaTrailing);
  int ignoreTrailing = 0;
  return (int)parseIntegerNumber(s, radix, tryOnly, typeChar, ignoreTrailing);
}


//virtual 
bool 
Integer::equals(IN(RObject) obj) 
{
  return obj != Nil && 
          instanceof(obj, Integer) != false && 
          RInteger(obj)->intValue() == intValue(); 
}

//static 
RClass 
Integer::getTYPE() 
{ 
  return Class::getSingeltonClass(dmi::ClazzInfo::getIntClazz());
}

//static 
RString 
Integer::toBinaryString(int val) 
{ 
 if (val < 0) {
    jlong lval = -val;
    lval += JLONG_CONSTANT(0x100000000);
    return String::_jltoa(lval, 2);
  }
  return String::_itoa(val, 2);
}

//static 
RString 
Integer::toHexString(int val) 
{ 
  if (val < 0) {
    jlong lval = -val;
    lval += JLONG_CONSTANT(0x100000000);
    return String::_jltoa(lval, 16);
  }
  return String::_itoa(val, 16);
}

//static 
RString 
Integer::toOctalString(int val) 
{ 
  if (val < 0) {
    jlong lval = -val;
    lval += JLONG_CONSTANT(0x100000000);
    return String::_jltoa(lval, 8);
  }
  return String::_itoa(val, 8);
}

} // lang
} // acdk
